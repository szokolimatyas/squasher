{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Squasher.Local where

import Squasher.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.Set (Set)
import Control.Monad.Trans.State.Strict
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.ByteString.Lazy (ByteString)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Generics.Uniplate.Operations (transformM)
import Data.Generics.Uniplate.Data
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Maybe
import Data.List (nub, intercalate, (\\), delete)
import Control.Monad (when, foldM)
import Data.Binary (decodeOrFail)
import Data.Binary.Get (ByteOffset)
import Control.Monad.Trans.Except (Except, throwE, except)
import Data.Foldable (traverse_)
import Foreign.Erlang.Term
import Debug.Trace
import Data.Tuple(swap)

newtype Path = MkPath { pathParts :: [PathPart] }
    deriving(Eq, Show)

data FunName = MkFunName { funName :: Text
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
        _ -> Nothing


entryFromTerm :: Term -> Except String (ErlType, Path)
entryFromTerm (Tuple [t1, t2]) = do
   erlTy <- except $ maybeToEither ("Not an erlang type: " ++ show t1) (fromTerm t1)
   path <- except $ maybeToEither ("Not a path: " ++ show t2) (fromTerm t2)
   return (erlTy, path)
entryFromTerm t = throwE $ "Not an entry: " ++ show t

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _def (Just val) = Right val
maybeToEither def Nothing = Left def

runner :: ByteString -> Except String SquashConfig
runner bs = case res of
    Right (_, _, MkExternalTerm (List terms Nil)) -> do
        entries <- mapM entryFromTerm terms
        let env = foldl (\tenv (t, p) -> update t p tenv) (MkTyEnv Map.empty) entries
        let env' = MkTyEnv (Map.take 20 $ unTyEnv env) --MkTyEnv (Map.take 1 $ unTyEnv env) --MkTyEnv (Map.take 1 $ Map.drop 14 (unTyEnv env))
        traceM $ "Env:\n" ++ show (Map.size $ unTyEnv env')
        return $ squashLocal env'
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

-- todo: binary/bitrstring
-- float/integer --> num?
-- none? any?
combine :: ErlType -> ErlType -> ErlType
combine EUnknown t = t
combine t EUnknown = t
combine t1 t2 | t1 == t2 = t1
combine EBitString EBinary = EBitString
combine EBinary EBitString = EBitString
combine (ENamedAtom a1) (ENamedAtom a2)
    | a1 `elem` ["true", "false"] &&
      a2 `elem` ["true", "false"] = EBoolean
-- not sure
combine (ETuple (EUnknown : ts1)) (ETuple (t2 : ts2)) | length ts1 == length ts2 =
    ETuple $ t2 : zipWith combine ts1 ts2
combine (ETuple (t1 : ts1)) (ETuple (EUnknown : ts2)) | length ts1 == length ts2 =
    ETuple $ t1 : zipWith combine ts1 ts2
combine (ETuple (ENamedAtom a1 : ts1)) (ETuple (ENamedAtom a2 : ts2)) | a1 == a2 && length ts1 == length ts2 =
    ETuple $ ENamedAtom a1 : zipWith combine ts1 ts2
combine (EList t1) (EList t2) = EList $ t1 `combine` t2
-- no support for shapemaps!
-- this is not the best, there could be too many different key types
combine (EMap m1) (EMap m2) =
    -- TODO: magic number
    -- collapse large maps
    if Map.size m1 + Map.size m2 < 5
    then EMap $ Map.unionWith combine m1 m2
    else EMap $ uncurry Map.singleton (combineMap $ Map.union m1 m2)
combine (EUnion ts) t1 = EUnion $ flattenUnions $ Set.map (`combine` t1) ts
combine t1 (EUnion ts) = EUnion $ flattenUnions $ Set.map (`combine` t1) ts
combine (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
    EFun (zipWith combine argts1 argts2) (combine t1 t2)
combine t1 t2 = EUnion $ flattenUnions $ Set.fromList [t1, t2]

combineMap :: Map ErlType ErlType -> (ErlType, ErlType)
combineMap = Map.foldrWithKey (\k' v' (k, v) -> (k `combine` k', v `combine` v')) (EUnknown, EUnknown)

combines :: [ErlType] -> ErlType
combines [] = EUnknown
combines (t:ts) = foldl combine t ts

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


-- Maybe instead of returning any() on too large unions, jut combine all of the entries
-- This should be good in cases like {'atom', {integer(), integer()}, <a bunch of atom literals>}
flattenUnions :: Set ErlType -> Set ErlType
flattenUnions s = if Set.size flatUnion > 20 then compactedUnion else flatUnion where
    flatUnion = Set.fold go Set.empty s
    go (EUnion ts) set = ts <> set
    go t set = Set.insert t set
    compactedUnion = Set.singleton (Set.fold lub ENone flatUnion)

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
                Nothing -> MkTyEnv $ Map.insert fn ty tenv
        _ -> error "Internal error"


data AliasEnv = MkAliasEnv 
    { aliasMap :: IntMap ErlType
    , nextIndex :: Int
    } deriving (Eq, Ord)

instance Show AliasEnv where
    show MkAliasEnv{..} =
        intercalate "\n" (map (\(i, t) -> "$" ++ show i ++ " -> " ++ show t) $ IntMap.toList aliasMap) ++
        "\n"

addAlias :: Int -> ErlType -> AliasEnv -> AliasEnv
addAlias i t MkAliasEnv{..} = MkAliasEnv (IntMap.insert i t aliasMap) nextIndex

-- not good: throw error!
lookupAlias :: Int -> AliasEnv -> ErlType
lookupAlias i MkAliasEnv{..} =
    Data.Maybe.fromMaybe (error "internal error") (IntMap.lookup i aliasMap)

data SquashConfig = SquashConfig
                  { aliasEnv :: AliasEnv
                  , tyEnv :: TyEnv
                  } deriving(Show)

addFunction :: FunName -> ErlType -> SquashConfig -> SquashConfig
addFunction fn t SquashConfig{tyEnv = MkTyEnv{..},..} =
    SquashConfig{tyEnv=MkTyEnv (Map.insert fn t unTyEnv), ..} 

reg :: SquashConfig -> ErlType -> (SquashConfig, ErlType)
reg SquashConfig{aliasEnv = MkAliasEnv{..}, ..} t =
    (SquashConfig{aliasEnv = MkAliasEnv (IntMap.insert nextIndex t aliasMap) (nextIndex + 1), ..}, EAliasMeta nextIndex)

resolve :: SquashConfig -> ErlType -> ErlType
resolve conf (EAliasMeta i) = lookupAlias i (aliasEnv conf)
resolve _    t = t

resolveUnions :: SquashConfig -> ErlType -> ErlType
resolveUnions conf t =
    let t' = resolve conf t in
    case t' of
        EUnion ts -> EUnion ts
        _ -> t

aliases :: ErlType -> [Int]
aliases t = Data.List.nub $ para visit t where
    visit :: ErlType -> [[Int]] -> [Int]
    visit (EAliasMeta i) is = i : concat is
    visit _ is = concat is

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
        filt _ = False
shouldMerge _ = False

mergeAliases :: SquashConfig -> [Int] -> SquashConfig
mergeAliases conf [] = conf
mergeAliases conf as@(a1:rest) = 
    mapAliasesTo (mapAliasesTo conf rest (EAliasMeta a1)) [a1] sigma where
        sigma = foldl1 combine $ map process as
        -- resolve alias -> remove top-level aliases -> change a_2..a_n refs to a_1
        -- we subst with rest, just in case there are references between a_n and and a_m, n and m > 1
        process ai = 
            substTy rest (EAliasMeta a1) (erase (resolve conf (EAliasMeta ai)))
        -- remove top-level aliases to avoid infinite types
        erase (EAliasMeta a') | a' `elem` as = EUnion Set.empty
        erase (EUnion ts) = EUnion $ flattenUnions $ Set.map erase ts
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

-- ????????????????????????????????????????????????????????
aliasTuple :: SquashConfig -> ErlType -> (SquashConfig, ErlType)
aliasTuple conf t = postwalk' conf t f where
    f conf' t'@(ETuple (ENamedAtom _ : _)) = reg conf' t'
    f conf' (EUnion ts) | any (\case { EAliasMeta _ -> True; _ -> False}) ts =
        reg conf' (EUnion $ Set.map (resolve conf') ts)
    f conf' t' = (conf', t') 

-- ????????????????????????????????????????????????????????,
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
squashLocal tEnv = Map.foldlWithKey h initialConf (unTyEnv tEnv) where
    initialConf = SquashConfig (MkAliasEnv IntMap.empty 0) (MkTyEnv Map.empty)

    h conf n t = addFunction n t1 conf2 where
        (conf1, t1) = aliasTuple conf t
        conf2 = squashAll conf1 t1

--     f :: ErlType -> State SquashConfig ErlType
--     f t@(ETuple (ENamedAtom _ : _)) = reg t
--   --  take 1 drop 14 takes a long time
--     -- f (EUnion ts) =
--     --     if any (\case { EAliasMeta _ -> True; _ -> False}) ts then do
--     --         ts' <- traverse resolveUnions (Set.toList ts)
--     --         reg $ EUnion $ Set.fromList ts'
--     --     else
--     --         return $ EUnion ts
--     f t = return t
-- -- | Substitute all occurrences of aliases in every alias
-- substInAliases :: [Int] -> ErlType -> State SquashConfig ()
-- substInAliases aliases newT = do
--     SquashConfig{aliases = MkAliasEnv imap} <- get
--     let newAliases = IntMap.map (substTy aliases newT) imap
--     modify (\conf -> conf{aliases = MkAliasEnv newAliases})
--     return ()

-- mapAliasesTo :: [Int] -> ErlType -> State SquashConfig ()
-- mapAliasesTo as newT = do
--     SquashConfig{aliases = MkAliasEnv imap} <- get
--     let newAliases = IntMap.mapWithKey
--                         (\i t -> if i `elem` as then newT else t)
--                         imap
--     modify (\conf -> conf{aliases = MkAliasEnv newAliases})
--     return ()

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

-- reg :: ErlType -> State SquashConfig ErlType
-- reg t = do
--     SquashConfig{counter=ctr} <- get
--     modify (\conf -> conf { counter = ctr+1
--                           , aliases = addAlias ctr t (aliases conf)
--                           })
--     return (EAliasMeta ctr)

-- resolve :: ErlType -> State SquashConfig ErlType
-- resolve (EAliasMeta i) = do
--     t <- gets (lookupAlias i . aliases)
--     resolve t
-- resolve t = return t

-- resolveUnions :: ErlType -> State SquashConfig ErlType
-- resolveUnions t = do
--     t' <- resolve t
--     case t' of
--         EUnion ts -> return $ EUnion ts
--         _ -> return t

-- aliasesInTy :: ErlType -> IntSet
-- aliasesInTy = para visit where
--     visit :: ErlType -> [IntSet] -> IntSet
--     visit (EAliasMeta i) is = IntSet.insert i (IntSet.unions is)
--     visit _ is = IntSet.unions is

-- -- | All aliases in a type, and the aliases in the aliases, etc...
-- aliasesInTyRec :: IntSet -> ErlType -> SquashConfig -> IntSet
-- aliasesInTyRec is t s = case t of
--     ETuple ts -> IntSet.unions $ map (\t' -> aliasesInTyRec is t' s) ts
--     EUnion ts -> IntSet.unions $ map (\t' -> aliasesInTyRec is t' s) (Set.toList ts)
--     EFun argts rest ->  IntSet.unions $ aliasesInTyRec is rest s : map (\t' -> aliasesInTyRec is t' s) argts
--     EList t' -> aliasesInTyRec is t' s
--     EMap m ->
--         IntSet.unions $ map (\(t1, t2) -> IntSet.union (aliasesInTyRec is t1 s) (aliasesInTyRec is t2 s)) $ Map.toList m
--     EAliasMeta i ->
--         if IntSet.member i is
--         then is
--         else
--             let t' = lookupAlias i (aliases s)
--                 is' = aliasesInTyRec (IntSet.insert i is) t' s
--             in is'
--     _ -> is

-- -- Modified from the paper a bit, unions are kept flat correctly.
-- -- But maybe if we do the old way, there is a pont to realiasing in the global step?
-- aliasTuples :: ErlType -> State SquashConfig ErlType
-- aliasTuples = postwalk f where
--     f :: ErlType -> State SquashConfig ErlType
--     f t@(ETuple (ENamedAtom _ : _)) = reg t
--   --  take 1 drop 14 takes a long time
--     -- f (EUnion ts) =
--     --     if any (\case { EAliasMeta _ -> True; _ -> False}) ts then do
--     --         ts' <- traverse resolveUnions (Set.toList ts)
--     --         reg $ EUnion $ Set.fromList ts'
--     --     else
--     --         return $ EUnion ts
--     f t = return t

-- pruneAliases :: State SquashConfig ()
-- pruneAliases = do
--     s <- get
--     let tys = Map.elems $ unTyEnv $ functions s
--     let usedAliases = IntSet.unions $ map (\t -> aliasesInTyRec IntSet.empty t s) tys
--     put s{aliases = MkAliasEnv (IntMap.restrictKeys (unAliasEnv $ aliases s) usedAliases)}

-- -- | Remove "proxy" aliases like $1 in: $1 -> $2 -> {'rec', integer()}
-- removeProxyAliases ::  State SquashConfig ()
-- removeProxyAliases = do
--     SquashConfig{aliases = MkAliasEnv as, functions = MkTyEnv funs} <- get
--     newAliases <- traverse remove as
--     newFuns <- traverse remove funs
--     modify (\conf -> conf{aliases = MkAliasEnv newAliases, functions = MkTyEnv newFuns})
--     return () where
--         remove = transformM resolveProxy

--         resolveProxy (EAliasMeta i) = do
--             t <- gets (lookupAlias i . aliases)
--             case t of
--                 EAliasMeta _ -> resolveProxy t
--                 _ -> return $ EAliasMeta i
--         resolveProxy t = return t

-- -- should this rather be a maybeMerge?
-- shouldMerge :: [ErlType] -> Bool
-- shouldMerge (ETuple (ENamedAtom n : elems) : ts) =
--     all filt ts where
--         filt :: ErlType -> Bool
--         filt (ETuple (ENamedAtom n' : elems')) =
--             n == n' && length elems == length elems'
--         filt _ = False
-- shouldMerge (ENamedAtom n : ts) =
--     all filt ts where
--         filt :: ErlType -> Bool
--         filt (ENamedAtom n') = n == n'
--         filt _ = False
-- shouldMerge _ = False

-- mergeAliases :: [Int] -> State SquashConfig ()
-- mergeAliases [] = return ()
-- mergeAliases ai@(a1:as) = do
--     ts <- traverse
--         (\i -> do
--             t <- erase <$> resolve (EAliasMeta i)
--             return $ substTy [i] (EAliasMeta a1) t
--         ) ai
--     let sigma = combines ts
--     mapAliasesTo as (EAliasMeta a1)
--     -- not sure about this one
--     substInAliases ai (EAliasMeta a1)
--     mapAliasesTo [a1] sigma
--     return () where
--         erase :: ErlType -> ErlType
--         erase (EAliasMeta a') | a' `elem` as = EUnion Set.empty
--         erase (EUnion ts) = EUnion $ flattenUnions $ Set.map erase ts
--         erase t = t

-- tryMergeAliases :: Int -> Int -> State SquashConfig ()
-- tryMergeAliases a1 a2 = do
--     t1 <- resolve $ EAliasMeta a1
--     t2 <- resolve $ EAliasMeta a2
--     let b = shouldMerge [t1, t2]
--     when b $ do
--         let t1' = substTy [a2] (EAliasMeta a1) (erase t1)
--         let t2' = substTy [a2] (EAliasMeta a1) (erase t2)
--         let sigma = t1' `combine` t2'
--         mapAliasesTo [a2] (EAliasMeta a1)
--         mapAliasesTo [a1] sigma
--     where
--         erase :: ErlType -> ErlType
--         erase (EAliasMeta a') | a' `elem` [a1, a2] = EUnion Set.empty
--         erase (EUnion ts) = EUnion $ Set.map erase ts
--         erase t = t

-- -- IntSets?
-- -- bad recursion scheme
-- squash :: [Int] -> IntSet -> State SquashConfig ()
-- squash [] _done = return ()
-- squash (a1 : worklist) done = do
--     SquashConfig{..} <- get
--     let as = aliasesInTy (lookupAlias a1 aliases) IntSet.\\ done
--     let ap = IntSet.delete a1 done
--     when (IntSet.notMember a1 done) (traverse_ f (IntSet.toList $ IntSet.union ap as))
--     squash (worklist ++ IntSet.toList as) (IntSet.insert a1 done)
--     where
--         f a2 = tryMergeAliases a1 a2


-- -- keep track of "done" variable, or variables we visited
-- -- group the aliases by tags (direct subalias, parent/done aliases)
-- -- merge the groups
-- -- -- postorder
-- -- mySquash :: Int -> State SquashConfig ()
-- -- mySquash alias = do
-- --     SquashConfig{..} <- get
-- --     let as = aliasesInTy (lookupAlias alias aliases) 
-- --     when (not $ IntSet.null as) $ do

-- --         _

-- -- this means that aliases that are under a common alias, on the same level,
-- -- get merged
-- data Tag = RecordTag Text Int | SingleAtom Text
--     deriving (Eq, Ord, Show)

-- groupByRecordTag :: [Int] -> State SquashConfig (Map Tag [Int])
-- groupByRecordTag = foldM visit Map.empty where
--     visit :: Map Tag [Int] -> Int -> State SquashConfig (Map Tag [Int])
--     visit acc i = do
--         SquashConfig{..} <- get
--         let t = lookupAlias i aliases
--         case t of
--             ETuple (ENamedAtom name : fs) ->
--                 return $ Map.alter (\case Just is -> Just (i : is); _ -> Just [i]) (RecordTag name (length fs)) acc
--                 --return $ Map.insert (RecordTag name (length fs)) i acc
--             ENamedAtom name ->
--                 return $ Map.alter (\case Just is -> Just (i : is); _ -> Just [i]) (SingleAtom name) acc
--             _ -> return acc

-- mySquashAll :: ErlType -> State SquashConfig ()
-- mySquashAll t = do
--     s <- get
--     let aliasesInT = IntSet.toList $ aliasesInTyRec IntSet.empty t s
--     groups <- groupByRecordTag aliasesInT
--     --traceM $ show
--     traverse_ mergeAliases groups


-- squashAll :: ErlType -> State SquashConfig ()
-- squashAll t =  traverse_ (\a -> squash [a] IntSet.empty) (IntSet.toList $ aliasesInTy t)

-- squashLocal :: State SquashConfig ()
-- squashLocal = do
--     (MkTyEnv e) <- gets functions
--     mapM_ h (Map.toList e)
--     removeProxyAliases
--     pruneAliases
--     SquashConfig{..} <- get
--     groups <- groupByRecordTag $ IntMap.keys $ unAliasEnv aliases
--     --traceM $ show
--     traverse_ mergeAliases groups 
--     removeProxyAliases
--     pruneAliases
--   --  s <- get
--    -- myTrace $ "Result:\n" ++ show s ++ "\n"
--     where
--         h :: (FunName, ErlType) -> State SquashConfig ()
--         h (x, t) = do
--             t' <- aliasTuples t
--           --  traceM $ "aliased type:\n" ++ show t' ++ "\n"
--          --   oldAliases <- gets aliases
--         --    traceM $ "show aliases:\n" ++ show oldAliases ++ "\n"
--             mySquashAll t'
--             SquashConfig{..} <- get
--             modify (\conf -> conf{functions=MkTyEnv $ Map.insert x t' (unTyEnv functions)})

-- -------------------------------------------------------------------------------
-- -- Global squashing
-- -------------------------------------------------------------------------------

-- -- In the base algo, the state change caused by postwalk is ignored
-- -- this might be a problem
-- singleRec :: ErlType -> State SquashConfig ErlType
-- singleRec = postwalk f where
--     f t'@(ETuple (ENamedAtom _ : _)) = reg t'
--     f t' = return t'

-- singleRec' :: FunName -> ErlType -> State SquashConfig ()
-- singleRec' fn t = do
--     sigma <- postwalk f t
--     modify (\s -> s{functions=MkTyEnv $ Map.insert fn sigma (unTyEnv $ functions s)})
--     return () where
--         f t'@(ETuple (ENamedAtom _ : _)) = reg t'
--         f t' = return t'

-- aliasSingleRec :: State SquashConfig ()
-- aliasSingleRec = do
--     s@SquashConfig{..} <- get
--     traverse_ (uncurry singleRec') (Map.toList $ unTyEnv functions)
--     aliases' <- traverse f (unAliasEnv aliases)
--     put s{aliases=MkAliasEnv aliases'}
--     where
--         f (EUnion ts) = do
--             ts' <- traverse singleRec $ Set.toList ts
--             return $ EUnion $ flattenUnions $ Set.fromList ts'
--         f t = singleRec t
