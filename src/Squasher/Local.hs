{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Squasher.Local (runner, SquashConfig(..), AliasEnv(..), TyEnv(..)) where

import           Control.Monad.Trans.State.Strict
import           Data.ByteString.Lazy             (ByteString)
import           Data.IntMap.Strict               (IntMap)
import           Data.IntSet                      (IntSet)
import qualified Data.IntSet                      as IntSet
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Squasher.Types
--import Data.Generics.Uniplate.Operations (transformM)
import           Control.Monad                    (zipWithM)
import           Control.Monad.Trans.Except       (Except, except, throwE)
import           Data.Binary                      (decodeOrFail)
import           Data.Binary.Get                  (ByteOffset)
import qualified Data.Equivalence.Monad           as Equiv
import           Data.Generics.Uniplate.Data
import qualified Data.IntMap.Strict               as IntMap
import           Data.List                        (delete, intercalate, nub,
                                                   (\\))
import qualified Data.Maybe
import           Data.Tuple                       (swap)
import           Debug.Trace
import           Foreign.Erlang.Term

newtype Path = MkPath { pathParts :: [PathPart] }
    deriving(Eq, Show)

data FunName = MkFunName { funName  :: Text
                         , funArity :: Int
                         } deriving(Eq, Ord)

instance Show FunName where
    show MkFunName{funName, funArity} =
        Text.unpack funName ++ "/" ++ show funArity

data PathPart =
                Dom Int Int -- ^ Parameter %1 of a function with arity %2
              | Range Int   -- ^ Result in a function with arity %1
              | FunN FunName -- ^ Function name and arity
              | Rec ErlType Int Int -- ^ Record with key, at position, field number
              | ListElement -- ^ List
              | MapElement ErlType -- ^ Not a shapemap yet!
              deriving(Eq, Show)

instance FromTerm PathPart where
    fromTerm t = case t of
        Tuple [Atom _ "rng", Integer i] -> Just $ Range $ fromInteger i
        Tuple [Atom _ "dom", Integer i, Integer j] -> Just $ Dom (fromInteger i) (fromInteger j)
        Tuple [Atom _ "name", String s, Integer i] ->
            Just $ FunN (MkFunName s (fromInteger i))
        Tuple [Atom _ "tuple_index", keyTerm, Integer i, Integer j] ->
            fromTerm keyTerm >>= \et -> Just $ Rec et (fromInteger i) (fromInteger j)
       -- Tuple [Atom _ "tuple_index", Tuple [Atom _ "atom", Atom _ key], Integer i, Integer j] ->
       --     Just $ Rec (Just key) (fromInteger i) (fromInteger j)
        Atom _ "list_element" -> Just ListElement
        Tuple [Atom _ "map_element", key] ->
            MapElement <$> fromTerm key
        -- TODO: what about when the tuple index is not an atom?
        -- Tuple [Atom _ "rec", Atom _ "undefined", Integer i, Integer j] ->
        --     Just $ Rec Nothing (fromInteger i) (fromInteger j)
        -- Tuple [Atom _ "rec", String key, Integer i, Integer j] ->
        --     Just $ Rec (Just key) (fromInteger i) (fromInteger j)
        _ -> Nothing

instance FromTerm Path where
    fromTerm t = case t of
        List elems _ -> MkPath <$> mapM fromTerm elems
        _            -> Nothing


entryFromTerm :: Term -> Except String (ErlType, Path)
entryFromTerm (Tuple [t1, t2]) = do
   erlTy <- except $ maybeToEither ("Not an erlang type: " ++ show t1) (fromTerm t1)
   path <- except $ maybeToEither ("Not a path: " ++ show t2) (fromTerm t2)
   return (erlTy, path)
entryFromTerm t = throwE $ "Not an entry: " ++ show t

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _def (Just val) = Right val
maybeToEither def Nothing     = Left def

runner :: ByteString -> Except String (SquashConfig, SquashConfig)
runner bs = case res of
    Right (_, _, MkExternalTerm (List terms Nil)) -> do
        entries <- mapM entryFromTerm terms
        traceM "Start update\n"
        let env = foldl (\tenv (t, p) -> update t p tenv) (MkTyEnv Map.empty) entries
        let env' = MkTyEnv (Map.take 20 $ unTyEnv env) --MkTyEnv (Map.take 1 $ unTyEnv env) --MkTyEnv (Map.take 1 $ Map.drop 14 (unTyEnv env))
        traceM $ "Env:\n" ++ show (Map.size $ unTyEnv env')
        let env'' = squashLocal env'
        return (env'', squashGlobal env'')
    Right (_, _, MkExternalTerm terms) -> throwE $ "Terms are in a wrong format: " ++ show terms
    Left (_, _, str) -> throwE $ "Could not parse bytestring, error: " ++ str
  where
    res ::  Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, ExternalTerm)
    res = decodeOrFail bs


newtype TyEnv = MkTyEnv { unTyEnv :: Map FunName ErlType }
instance Show TyEnv where
    show (MkTyEnv m) =
        intercalate "\n" (map (\(i, t) -> show i ++ " -> " ++ show t) $ Map.toList m) ++
        "\n"

-- Combine, may introduce toplevel unions
combine :: ErlType -> ErlType -> ErlType
combine t1 t2 | Just t3 <- equate t1 t2 = t3
combine (ENamedAtom a1) (ENamedAtom a2)
    | a1 `elem` ["true", "false"] &&
      a2 `elem` ["true", "false"] = EBoolean
combine (EList t1) (EList t2) = EList $ t1 `combine` t2
-- no support for shapemaps!
-- this is not the best, there could be too many different key types
combine (EMap m1) (EMap m2) = EMap $ Map.unionWith combine m1 m2
combine (EUnion ts) t1 | Set.null ts = t1
combine t1 (EUnion ts) | Set.null ts = t1
combine (EUnion ts1) (EUnion ts2) = mkFlatUnion $ Set.fold (flip combineUnion) ts1 ts2
combine (EUnion ts) t1 = mkFlatUnion $ combineUnion ts t1
combine t1 (EUnion ts) = mkFlatUnion $ combineUnion ts t1
combine (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
    EFun (zipWith combine argts1 argts2) (combine t1 t2)
-- adding combineUnion here breaks stuff for some reason
combine t1 t2 = mkFlatUnion $ Set.fromList [t1, t2]

-- todo: better performance??
combineUnion :: Set ErlType -> ErlType -> Set ErlType
combineUnion ts t = if didEquate then newTs else Set.map (`combine` t) ts where
    (newTs, didEquate) = Set.fold go (Set.empty, False) $ Set.fold elements Set.empty ts 

    elements (EUnion ts') set = ts' <> set
    elements t' set           = Set.insert t' set

    go t' (ts', combined') = 
        case equate t t' of
            Just t'' -> (Set.insert t'' ts', True)
            Nothing -> (Set.insert t' ts', combined') 

-- Combine, returns Nothing instead of toplevel unions or upcasts
equate :: ErlType -> ErlType -> Maybe ErlType
equate t1 t2 | t1 == t2 = Just t1
equate EUnknown t1 = Just t1
equate t1 EUnknown = Just t1
equate _ EAny = Just EAny
equate EAny _ = Just EAny
equate t1@(ENamedAtom _) EAnyAtom = Just t1
equate EAnyAtom t1@(ENamedAtom _) = Just t1
equate EBoolean EAnyAtom = Just EAnyAtom
equate EAnyAtom EBoolean = Just EAnyAtom
equate (ENamedAtom txt) EBoolean | txt == "true" || txt == "false" = Just EBoolean
equate EBoolean (ENamedAtom txt) | txt == "true" || txt == "false" = Just EBoolean
equate EBitString EBinary = Just EBitString
equate EBinary EBitString = Just EBitString
equate (EList t1) (EList t2) = EList <$> equate t1 t2
-- what is with record like structures?,
equate (ETuple ts1) (ETuple ts2) | length ts1 /= length ts2 = Nothing
equate (ETuple (ENamedAtom a1 : ts1)) (ETuple (ENamedAtom a2 : ts2)) | a1 == a2 =
    Just $ ETuple $ ENamedAtom a1 : zipWith combine ts1 ts2
equate (ETuple ts1) (ETuple ts2) =
    ETuple <$> zipWithM equate ts1 ts2
--equate (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
--    Just $ EFun (zipWith combine argts1 argts2) (combine t1 t2)
equate _ _ = Nothing

-- Function used to tame too large types, collaping them.
-- Not sure if the handling of Unknown values here is correct
-- should it be promoted to any or unknown or ignored?
-- float/integer --> num?
lub :: ErlType -> ErlType -> ErlType
lub ENone t2 = t2
lub t1 ENone = t1
lub EUnknown _ = EAny
lub _ EUnknown = EAny
lub t1 t2 | t1 == t2 = t1
lub EBitString EBinary = EBitString
lub EBinary EBitString = EBitString
lub t1 t2
    | t1 `elem` [EBoolean, ENamedAtom "true", ENamedAtom "false"] &&
      t2 `elem` [EBoolean, ENamedAtom "true", ENamedAtom "false"] = EBoolean
lub (ETuple ts1) (ETuple ts2)| length ts1 == length ts2 =
    ETuple $ zipWith lub ts1 ts2
lub (EList t1) (EList t2) = EList $ t1 `lub` t2
-- TODO: actual lub for maps
lub (EMap _) (EMap _) =
    EMap $ Map.singleton EAny EAny
-- TODO: is there a need for greatest lower bound here?
-- what exactly is the type hierarchy
lub (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
    EFun (replicate (length argts1) ENone) (lub t1 t2)
lub _ _ = EAny


mkFlatUnion :: Set ErlType -> ErlType
mkFlatUnion ts | Set.size flatUnion > 60 = Set.fold lub ENone flatUnion
               | otherwise = EUnion flatUnion where
    flatUnion = squashUnionElements $ Set.fold go Set.empty ts

    go (EUnion ts') set = ts' <> set
    go t' set           = Set.insert t' set

makeArgs :: ErlType -> Int -> Int -> [ErlType]
makeArgs t index size =
    [ getT i | i <- [1 .. size] ] where
        getT i | i == index = t
               | otherwise = EUnknown

update :: ErlType -> Path -> TyEnv -> TyEnv
update ty (MkPath p) env =
    case p of
        -- Not a record-like tuple, use first element's type
        Rec _ 1 size : p' ->
            update (ETuple $ ty : replicate (size-1) EUnknown)
                   (MkPath p') env
        Rec k index size : p' ->
            update (ETuple $ k : makeArgs ty (index-1) (size-1))
                   (MkPath p') env
        Dom pos arity : p' ->
            update (EFun (makeArgs ty pos arity) EUnknown) (MkPath p') env
        Range arity : p' ->
            update (EFun (replicate arity EUnknown) ty) (MkPath p') env
        ListElement : p' ->
            update (EList ty) (MkPath p') env
        MapElement k : p' ->
            update (EMap $ Map.singleton k ty) (MkPath p') env
        [FunN fn] -> let tenv = unTyEnv env in
            case Map.lookup fn tenv of
                Just ty' -> MkTyEnv $ Map.insert fn (combine ty ty') tenv
                Nothing  -> MkTyEnv $ Map.insert fn ty tenv
        _ -> error "Internal error"


data AliasEnv = MkAliasEnv
    { aliasMap  :: IntMap ErlType
    , nextIndex :: Int
    } deriving (Eq, Ord)

instance Show AliasEnv where
    show MkAliasEnv{..} =
        intercalate "\n" (map (\(i, t) -> "$" ++ show i ++ " -> " ++ show t) $ IntMap.toList aliasMap) ++
        "\n"

-- not good: throw error!
lookupAlias :: Int -> AliasEnv -> ErlType
lookupAlias i MkAliasEnv{..} =
    Data.Maybe.fromMaybe (error "internal error") (IntMap.lookup i aliasMap)

data SquashConfig = SquashConfig
                  { aliasEnv :: AliasEnv
                  , tyEnv    :: TyEnv
                  } deriving(Show)

addFunction :: FunName -> ErlType -> SquashConfig -> SquashConfig
addFunction fn t SquashConfig{tyEnv = MkTyEnv{..},..} =
    SquashConfig{tyEnv=MkTyEnv (Map.insert fn t unTyEnv), ..}


addAlias :: Int -> ErlType -> SquashConfig -> SquashConfig
addAlias i t SquashConfig{aliasEnv=MkAliasEnv{..},..} =
    SquashConfig{aliasEnv=MkAliasEnv (IntMap.insert i t aliasMap) nextIndex, ..}


reg :: SquashConfig -> ErlType -> (SquashConfig, ErlType)
reg SquashConfig{aliasEnv = MkAliasEnv{..}, ..} t =
    (SquashConfig{aliasEnv = MkAliasEnv (IntMap.insert nextIndex t aliasMap) (nextIndex + 1), ..}, EAliasMeta nextIndex)

resolve :: SquashConfig -> ErlType -> ErlType
resolve conf (EAliasMeta i) = lookupAlias i (aliasEnv conf)
resolve _    t              = t

aliases :: ErlType -> [Int]
aliases t = Data.List.nub $ para visit t where
    visit :: ErlType -> [[Int]] -> [Int]
    visit (EAliasMeta i) is = i : concat is
    visit _ is              = concat is

shouldMerge :: [ErlType] -> Bool
shouldMerge (ETuple (ENamedAtom n : elems) : ts) =
    all filt ts where
        filt :: ErlType -> Bool
        filt (ETuple (ENamedAtom n' : elems')) =
            n == n' && length elems == length elems'
        filt _ = False
shouldMerge (ENamedAtom n : ts) =
    all filt ts where
        filt :: ErlType -> Bool
        filt (ENamedAtom n') = n == n'
        filt _               = False
shouldMerge _ = False

mergeAliases :: SquashConfig -> [Int] -> SquashConfig
mergeAliases conf [] = conf
mergeAliases conf [_] = conf
mergeAliases conf as@(a1:rest) =
    mapAliasesTo (mapAliasesTo conf rest (EAliasMeta a1)) [a1] sigma where
        sigma = foldl1 combine $ map process as
        -- resolve alias -> remove top-level aliases -> change a_2..a_n refs to a_1
        -- we subst with rest, just in case there are references between a_n and and a_m, n and m > 1
        process ai =
            substTy rest (EAliasMeta a1) (erase (resolve conf (EAliasMeta ai)))
        -- remove top-level aliases to avoid infinite types
        erase (EAliasMeta a') | a' `elem` as = EUnion Set.empty
        erase (EUnion ts) = mkFlatUnion $ Set.map erase ts
        erase t = t

-- -- | Change all occurences of aliases to newT in a type
substTy :: [Int] -> ErlType -> ErlType -> ErlType
substTy as newT = transform go where
    go (EAliasMeta alias) | alias `elem` as = newT
    go t = t

mapAliasesTo :: SquashConfig -> [Int] -> ErlType -> SquashConfig
mapAliasesTo SquashConfig{aliasEnv=MkAliasEnv{..},..} as newT =
    SquashConfig{aliasEnv=MkAliasEnv{aliasMap=newAliases, ..}, ..} where
        newAliases :: IntMap ErlType
        newAliases =
            IntMap.mapWithKey
                (\i t -> if i `elem` as then newT else t)
                aliasMap

aliasTuple :: SquashConfig -> ErlType -> (SquashConfig, ErlType)
aliasTuple conf t = postwalk' conf t f where
 --   f conf' t'@(ENamedAtom _) = reg conf' t'
    f conf' t'@(ETuple (ENamedAtom _ : _)) = reg conf' t'
    f conf' (EUnion ts) | any (\case { EAliasMeta _ -> True; _ -> False}) ts =
        reg conf' (EUnion $ Set.map (resolve conf') ts)
    f conf' t' = (conf', t')

squash :: SquashConfig -> [Int] -> [Int] -> SquashConfig
squash conf []     _d = conf
squash conf@SquashConfig{..} (a1:w) d  = squash conf' (w ++ as) (d ++ [a1]) where
    as = aliases (lookupAlias a1 aliasEnv) Data.List.\\ d
    ap = Data.List.delete a1 d
    -- refers to ai, what is it???
    f delta a2 =
        if not (shouldMerge (map (resolve delta) [EAliasMeta a1, EAliasMeta a2]))
        then delta
        else mergeAliases delta [a1, a2] -- or include the whole worklist?

    conf' =
        if a1 `elem` d then conf else foldl f conf (ap ++ as)

squashAll :: SquashConfig -> ErlType -> SquashConfig
squashAll conf0 t = confn where
    as = aliases t
    confn = foldl (\conf' i -> squash conf' [i] []) conf0 as

squashLocal :: TyEnv -> SquashConfig
squashLocal tEnv = pruneAliases $ removeProxyAliases $ Map.foldlWithKey h initialConf (unTyEnv tEnv) where
    initialConf = SquashConfig (MkAliasEnv IntMap.empty 0) (MkTyEnv Map.empty)

    h conf n t = addFunction n t1 conf2 where
        (conf1, t1) = aliasTuple conf t
        conf2 = squashAll conf1 t1

-- just until the debugging is done...
postwalk' :: SquashConfig -> ErlType ->
             (SquashConfig -> ErlType -> (SquashConfig, ErlType)) ->
             (SquashConfig, ErlType)
postwalk' conf t f = swap $ runState (postwalk f' t) conf where
    f' t' = state (\conf' -> swap $ f conf' t')

postwalk :: (ErlType -> State a ErlType) ->
            ErlType ->
            State a ErlType
postwalk = transformM

-- | Remove single element unions, collapse nested unions
removeSingleUnions :: SquashConfig -> SquashConfig
removeSingleUnions conf@SquashConfig{aliasEnv = MkAliasEnv{..}, tyEnv = MkTyEnv funs} =
    conf {aliasEnv = MkAliasEnv newAliasMap nextIndex, tyEnv = MkTyEnv newFuns} where
        newAliasMap = IntMap.map flattenUnions aliasMap
        newFuns = Map.map flattenUnions funs

        flattenUnions :: ErlType -> ErlType
        flattenUnions = transform visit

        visit :: ErlType -> ErlType
        visit (EUnion ts) | Set.size ts == 1 = Set.findMin ts
        visit (EUnion ts) = EUnion $ Set.fold go Set.empty ts
        visit t = t

        go (EUnion ts) set = ts <> set
        go t set           = Set.insert t set

-- | Remove single element unions, collapse nested unions
-- Maybe we could leave only this, and don't use equate when combining things
-- Mainly depends on performance impact I think

-- And why do we even need this with the combine function already using equate for unions?
tryRemoveUnknowns :: SquashConfig -> SquashConfig
tryRemoveUnknowns conf@SquashConfig{aliasEnv = MkAliasEnv{..}, tyEnv = MkTyEnv funs} =
    conf {aliasEnv = MkAliasEnv newAliasMap nextIndex, tyEnv = MkTyEnv newFuns} where
        newAliasMap = IntMap.map equateElements aliasMap
        newFuns = Map.map equateElements funs

        equateElements :: ErlType -> ErlType
        equateElements = transform visit

        visit :: ErlType -> ErlType
        visit (EUnion ts) = EUnion $ Set.fold doRemove Set.empty ts
        visit t = t

        doRemove :: ErlType -> Set ErlType -> Set ErlType
        doRemove t ts = if didEquate then newTs else Set.insert t newTs where

            (newTs, didEquate) = Set.fold go (Set.empty, False) ts

            go t' (ts', combined') = 
                case equate t t' of
                    Just t'' -> (Set.insert t'' ts', True)
                    Nothing -> (Set.insert t' ts', combined') 


-- | Remove "proxy" aliases like $1 in: $1 -> $2 -> {'rec', integer()}
removeProxyAliases :: SquashConfig -> SquashConfig
removeProxyAliases conf@SquashConfig{aliasEnv = MkAliasEnv{..}, tyEnv = MkTyEnv funs} =
    conf {aliasEnv = MkAliasEnv newAliasMap nextIndex, tyEnv = MkTyEnv newFuns} where
        newAliasMap = IntMap.map (remove conf) aliasMap
        newFuns = Map.map (remove conf) funs

        remove conf' t = snd $ postwalk' conf' t resolveProxy

        resolveProxy conf' (EAliasMeta i) =
            case lookupAlias i (aliasEnv conf') of
                EAliasMeta j -> resolveProxy conf' (EAliasMeta j)
                _            -> (conf', EAliasMeta i)
        resolveProxy conf' t = (conf', t)


-- | Remove mappings for unused aliases.
pruneAliases :: SquashConfig -> SquashConfig
pruneAliases conf@SquashConfig{aliasEnv = MkAliasEnv{..},tyEnv = MkTyEnv funs} =
    conf {aliasEnv = MkAliasEnv newAliasMap nextIndex } where

    tys = Map.elems funs
    usedAliases = IntSet.unions $ map (\t -> aliasesInTyRec IntSet.empty t conf) tys
    newAliasMap = IntMap.restrictKeys aliasMap usedAliases

-- | All aliases in a type, and the aliases in the aliases, etc...
aliasesInTyRec :: IntSet -> ErlType -> SquashConfig -> IntSet
aliasesInTyRec is t s = case t of
    ETuple ts -> IntSet.unions $ map (\t' -> aliasesInTyRec is t' s) ts
    EUnion ts -> IntSet.unions $ map (\t' -> aliasesInTyRec is t' s) (Set.toList ts)
    EFun argts rest ->  IntSet.unions $ aliasesInTyRec is rest s : map (\t' -> aliasesInTyRec is t' s) argts
    EList t' -> aliasesInTyRec is t' s
    EMap m ->
        IntSet.unions $ map (\(t1, t2) -> IntSet.union (aliasesInTyRec is t1 s) (aliasesInTyRec is t2 s)) $ Map.toList m
    EAliasMeta i ->
        if IntSet.member i is
        then is
        else
            let t' = lookupAlias i (aliasEnv s)
                is' = aliasesInTyRec (IntSet.insert i is) t' s
            in is'
    _ -> is

-- -------------------------------------------------------------------------------
-- -- Global squashing
-- -------------------------------------------------------------------------------

-- originally conf' was not used, which is weird
singleRecFun :: SquashConfig -> FunName -> ErlType -> SquashConfig
singleRecFun conf fn t = addFunction fn t' conf' where
    (conf', t') = postwalk' conf t f

    --f co ty@(ENamedAtom _) = reg co ty
    f co ty@(ETuple (ENamedAtom _ : _)) = reg co ty
    f co ty                             = (co, ty)

singleRecAlias :: SquashConfig -> ErlType -> (SquashConfig, ErlType)
singleRecAlias conf t = (conf', t') where
    (conf', t') = postwalk' conf t f

    --f co ty@(ENamedAtom _) = reg co ty
    f co ty@(ETuple (ENamedAtom _ : _)) = reg co ty
    f co ty                             = (co, ty)


-- Bit weird
aliasSingleRec :: SquashConfig -> SquashConfig
aliasSingleRec conf = IntMap.foldlWithKey f conf' (aliasMap $ aliasEnv conf') where
    conf' = Map.foldlWithKey singleRecFun conf (unTyEnv $ tyEnv conf)

    f conf0 a (ETuple (ENamedAtom txt : ts)) =
        let
            (confn, ts') = foldChildren ts conf0
        in
            addAlias a (ETuple (ENamedAtom txt : ts')) confn
    f conf0 a t =
        let
            (conf1, t1) = singleRecAlias conf0 t
        in
            addAlias a t1 conf1

foldChildren :: [ErlType] -> SquashConfig -> (SquashConfig, [ErlType])
foldChildren ts conf = (conf', reverse ts') where
    (conf', ts') = foldl visit (conf, []) ts

    visit (c, acc) ty =
        let (c1, ty1) = singleRecAlias c ty in
        (c1, ty1 : acc)

data Tag = RecordTag Text Int | SingleAtom Text
    deriving (Eq, Ord, Show)

tag :: SquashConfig -> ErlType -> Maybe Tag
tag conf ty = case resolve conf ty of
    (ETuple (ENamedAtom txt : ts)) -> Just $ RecordTag txt $ length ts
    ENamedAtom txt -> Just $ SingleAtom txt
    EUnion ts ->
        case map (tag conf) $ Set.toList ts of
            t':ts' ->
                if all (t'==) ts' then t' else Nothing
            _ -> Nothing
    _ -> Nothing

groupSimilarRecs :: SquashConfig -> Map Tag [Int]
groupSimilarRecs conf@SquashConfig{aliasEnv=MkAliasEnv aliasMap _} =
    IntMap.foldlWithKey visit Map.empty aliasMap where

    visit acc key _ =
        case tag conf (EAliasMeta key) of
            Just theTag ->
                Map.alter (\case Just is -> Just (key : is); _ -> Just [key]) theTag acc
            _ -> acc

squashUnionElements :: Set ErlType -> Set ErlType
squashUnionElements ts = unTaggedSet <> Set.fromList (Map.elems tagMap) where
    (unTaggedSet, tagMap) = Set.foldl visit (Set.empty, Map.empty) ts

    visit (unTagged, tagged) t =
        case immediateTag t of
            Just theTag ->
                (unTagged, Map.alter (\case Just t' -> Just $ t `combine` t'; _ -> Just t) theTag tagged)
            _ -> (Set.insert t unTagged, tagged)

    immediateTag :: ErlType -> Maybe Tag
    immediateTag t = case t of
        (ETuple (ENamedAtom txt : rest)) -> Just $ RecordTag txt $ length rest
        ENamedAtom txt                   -> Just $ SingleAtom txt
        _                                -> Nothing

squashHorizontally :: SquashConfig -> SquashConfig
squashHorizontally conf = Map.foldl mergeAliases conf (groupSimilarRecs conf)

squashHorizontallyMulti :: SquashConfig -> SquashConfig
squashHorizontallyMulti conf = foldl mergeAliases conf (getEq (aliasesToTags conf) conf)

tagMulti :: SquashConfig -> ErlType -> Set Tag
tagMulti conf ty = case resolve conf ty of
    (ETuple (ENamedAtom txt : ts)) -> Set.singleton $ RecordTag txt $ length ts
    ENamedAtom txt                 -> Set.singleton $ SingleAtom txt
    EUnion ts                      -> Set.unions $ Set.map (tagMulti conf) ts
    _                              -> Set.empty

-- groupSimilarRecsMulti :: SquashConfig -> Map (Set Tag) [Int]
-- groupSimilarRecsMulti conf@SquashConfig{aliasEnv=MkAliasEnv aliasMap _} =
--     Map.map Data.List.nub groups where

--     groups = IntMap.foldlWithKey visit Map.empty aliasMap

--     visit acc key _ = let tags = tagMulti conf (EAliasMeta key) in
--         if not (Set.null tags)
--         then Map.alter (\case Just is -> Just (key : is); _ -> Just [key]) tags acc
--         else acc

type TagEq s a = Equiv.EquivM s (Set Tag) Int a

-- Might be better if the tags were precomputed?
-- also, what happens with the Set.null tagged aliases?
runEq :: SquashConfig -> (forall s. TagEq s a) -> a
runEq conf = Equiv.runEquivM (tagMulti conf . EAliasMeta) Set.union

getEq :: Map Tag [Int] -> SquashConfig -> [[Int]]
getEq tagMap conf = runEq conf $ do
    mapM_ visit tagMap
    clss <- Equiv.classes
    traverse classesToAliases clss where
        visit :: [Int] -> TagEq s ()
        visit = Equiv.equateAll

        classesToAliases cl = do
            tags <- Equiv.desc cl
            let l = foldl (\as tg -> Map.findWithDefault [] tg tagMap ++ as) [] tags
            return $ Data.List.nub l


aliasesToTags :: SquashConfig -> Map Tag [Int]
aliasesToTags conf@SquashConfig{aliasEnv=MkAliasEnv aliasMap _} =
    Map.map Data.List.nub groups where

    groups = IntMap.foldlWithKey visit Map.empty aliasMap

    visit acc alias _ =
        Set.fold (addTag alias) acc (tagMulti conf (EAliasMeta alias))

    addTag alias = Map.alter (\case Just is -> Just (alias : is); _ -> Just [alias])

-- Clean up the multiple uses of removeSingleUnions, why do we need multiples of them?
-- Could we unify proxy removal, pruning, etc?
squashGlobal :: SquashConfig -> SquashConfig
squashGlobal = compose [ aliasSingleRec
                       , removeSingleUnions
                       -- horizontal squash, single
                       , squashHorizontally
                       , removeSingleUnions
                       , removeProxyAliases
                       , pruneAliases
                       , removeSingleUnions
                       -- horizontal squash, multi
                       , squashHorizontallyMulti
                       , removeSingleUnions
                       , removeProxyAliases
                       , pruneAliases
                       , removeSingleUnions
                       , removeProxyAliases
                       , pruneAliases
                       , removeSingleUnions
                       , tryRemoveUnknowns
                       ]

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id
