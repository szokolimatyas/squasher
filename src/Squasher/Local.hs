{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Squasher.Local (runner, SquashConfig(..), AliasEnv(..), TyEnv(..)) where

import           Control.Monad.Trans.State.Strict
import           Data.ByteString.Lazy                    (ByteString)
import           Data.HashSet                            (HashSet)
import           Data.IntMap.Strict                      (IntMap)
import           Data.IntSet                             (IntSet)
import qualified Data.IntSet                             as IntSet
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Data.Set                                (Set)
import qualified Data.Set                                as Set
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import           Squasher.Types
--import Data.Generics.Uniplate.Operations (transformM)
import           Algebra.Graph.AdjacencyIntMap           (AdjacencyIntMap)
import qualified Algebra.Graph.AdjacencyIntMap
import           Algebra.Graph.AdjacencyIntMap.Algorithm (bfs)
import           Control.Monad                           (zipWithM)
import qualified Control.Monad.ST.Trans                  as STT
import           Control.Monad.Trans.Except              (Except, except,
                                                          throwE)
import           Data.Binary                             (decodeOrFail)
import           Data.Binary.Get                         (ByteOffset)
import           Data.Containers.ListUtils               (nubInt, nubOrd)
import qualified Data.Equivalence.STT                    as Equiv
import           Data.Foldable                           (foldl')
import           Data.Functor.Identity                   (runIdentity)
import           Data.Generics.Uniplate.Data
import qualified Data.HashSet                            as HashSet
import qualified Data.IntMap.Strict                      as IntMap
import           Data.List                               (intercalate)
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

runner :: ByteString -> Except String SquashConfig
runner bs = case dec of
    Right (_, _, MkExternalTerm (List terms Nil)) -> do
        entries <- mapM entryFromTerm terms
        traceM "Start update!"
        let env = foldl (\tenv (t, p) -> update t p tenv) (MkTyEnv Map.empty) entries
       -- let env' = MkTyEnv (Map.take 20 $ unTyEnv env) --MkTyEnv (Map.take 1 $ unTyEnv env) --MkTyEnv (Map.take 1 $ Map.drop 14 (unTyEnv env))
        let env' = env
        traceM $ "Env:\n" ++ show (Map.size $ unTyEnv env')
        let env'' = Debug.Trace.trace "Local squash done, start global squash!" squashLocal env'
        let res = squashGlobal env''
        return res
    Right (_, _, MkExternalTerm terms) -> throwE $ "Terms are in a wrong format: " ++ show terms
    Left (_, _, str) -> throwE $ "Could not parse bytestring, error: " ++ str
  where
    dec ::  Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, ExternalTerm)
    dec = decodeOrFail bs


newtype TyEnv = MkTyEnv { unTyEnv :: Map FunName ErlType }
instance Show TyEnv where
    show (MkTyEnv m) =
        intercalate "\n" (map showFun $ Map.toList m) ++ "\n" where

        showFun (MkFunName{funName}, EFun ts t) = Text.unpack funName ++ "(" ++ intercalate ", " (map show ts) ++ ") -> " ++ show t
        showFun (MkFunName{funName,funArity}, _t) = Text.unpack funName ++ "/" ++ show funArity ++  ": no text available"

-- Combine, may introduce toplevel unions
combine :: ErlType -> ErlType -> ErlType
combine t1 t2 | Just t3 <- equate t1 t2 = t3
combine (ENamedAtom a1) (ENamedAtom a2)
    | a1 `elem` ["true", "false"] &&
      a2 `elem` ["true", "false"] = EBoolean
-- no support for shapemaps!
-- this is not the best, there could be too many different key types
combine (EMap m1) (EMap m2) = EMap $ Map.unionWith combine m1 m2
combine (EUnion ts) t1 | HashSet.null ts = t1
combine t1 (EUnion ts) | HashSet.null ts = t1
combine (EUnion ts1) (EUnion ts2) = mkFlatUnion $ HashSet.foldl' combineUnion ts1 ts2 -- (flip combineUnion)
combine (EUnion ts) t1 = mkFlatUnion $ combineUnion ts t1
combine t1 (EUnion ts) = mkFlatUnion $ combineUnion ts t1
combine (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
    EFun (zipWith combine argts1 argts2) (combine t1 t2)
combine t1 t2 = mkFlatUnion $ combineUnion (HashSet.singleton t1) t2

-- todo: better performance??
-- accumulation needs to be faster
combineUnion :: HashSet ErlType -> ErlType -> HashSet ErlType
combineUnion ts t = if didEquate then newTs else HashSet.insert t ts where
    (newTs, didEquate) = HashSet.foldl' go (HashSet.empty, False) $ HashSet.foldl' elements HashSet.empty ts

    elements set (EUnion ts') = ts' <> set
    elements set t'           = HashSet.insert t' set

    go (ts', combined') t' =
        case equate t t' of
            Just t'' -> (HashSet.insert t'' ts', True)
            Nothing  -> (HashSet.insert t' ts', combined')

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
-- TODO: when equating, transform inner representations to container
equate (EContainer c1) (EContainer c2) = EContainer <$> equateCont c1 c2
-- what is with record like structures?,
equate (ETuple ts1) (ETuple ts2) | length ts1 /= length ts2 = Nothing
equate (ETuple (ENamedAtom a1 : ts1)) (ETuple (ENamedAtom a2 : ts2)) | a1 == a2 =
    Just $ ETuple $ ENamedAtom a1 : zipWith combine ts1 ts2
equate (ETuple ts1) (ETuple ts2) =
    ETuple <$> zipWithM equate ts1 ts2
--equate (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
--    Just $ EFun (zipWith combine argts1 argts2) (combine t1 t2)
equate _ _ = Nothing

-- should this rather be combine??
equateCont :: Container ErlType -> Container ErlType -> Maybe (Container ErlType)
equateCont (CList t1) (CList t2)     = Just $ CList $ combine t1 t2
equateCont (CDict t1) (CDict t2)     = Just $ CDict $ combine t1 t2
equateCont (COldSet t1) (COldSet t2) = Just $ COldSet $ combine t1 t2
equateCont (CGbSet t1) (CGbSet t2)   = Just $ CGbSet $ combine t1 t2
equateCont (CGbTree t1 t2) (CGbTree t3 t4) = Just $ CGbTree (combine t1 t3) (combine t2 t4)
equateCont CGb CGb = Just CGb
equateCont CGb (CGbSet t1) = Just $ CGbSet t1
equateCont (CGbSet t1) CGb = Just $ CGbSet t1
equateCont CGb (CGbTree t1 t2) = Just $ CGbTree t1 t2
equateCont (CGbTree t1 t2) CGb = Just $ CGbTree t1 t2
equateCont (CArray t1) (CArray t2) = Just $ CArray $ combine t1 t2
equateCont _ _ = Nothing

-- Function used to tame too large types, collaping them.
-- Not sure if the handling of Unknown values here is correct
-- should it be promoted to any or unknown or ignored?
-- float/integer --> num?
lub :: ErlType -> ErlType -> ErlType
lub t1 t2 | Just t3 <- equate t1 t2 = t3
lub ENone t2 = t2
lub t1 ENone = t1
lub (ETuple ts1) (ETuple ts2)| length ts1 == length ts2 =
    ETuple $ zipWith lub ts1 ts2
lub (EContainer c1) (EContainer c2) = lubCont c1 c2
-- TODO: actual lub for maps
lub (EMap _) (EMap _) =
    EMap $ Map.singleton EAny EAny
-- TODO: is there a need for greatest lower bound here?
-- what exactly is the type hierarchy
lub (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
    EFun (replicate (length argts1) ENone) (lub t1 t2)
lub _ _ = EAny

lubCont :: Container ErlType -> Container ErlType -> ErlType
lubCont (CList t1) (CList t2)     = EContainer $ CList $ lub t1 t2
lubCont (CDict t1) (CDict t2)     = EContainer $ CDict $ lub t1 t2
lubCont (COldSet t1) (COldSet t2) = EContainer $ COldSet $ lub t1 t2
lubCont (CGbSet t1) (CGbSet t2)   = EContainer $ CGbSet $ lub t1 t2
lubCont (CGbTree t1 t2) (CGbTree t3 t4) = EContainer $ CGbTree (lub t1 t3) (lub t2 t4)
lubCont CGb CGb = EContainer CGb
lubCont CGb (CGbSet t1) = EContainer $ CGbSet t1
lubCont (CGbSet t1) CGb = EContainer $ CGbSet t1
lubCont CGb (CGbTree t1 t2) = EContainer $ CGbTree t1 t2
lubCont (CGbTree t1 t2) CGb = EContainer $ CGbTree t1 t2
lubCont (CArray t1) (CArray t2) = EContainer $ CArray $ lub t1 t2
lubCont _ _ = EAny


mkFlatUnion :: HashSet ErlType -> ErlType
mkFlatUnion ts | HashSet.size flatUnion > 100 = HashSet.foldl' lub ENone flatUnion
               | otherwise = EUnion flatUnion where
--    flatUnion = squashUnionElements $ Set.fold go Set.empty ts
    flatUnion = HashSet.foldl' go HashSet.empty ts


    go set (EUnion ts') = ts' <> set
    go set t'           = HashSet.insert t' set

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
            update (EContainer $ CList ty) (MkPath p') env
        MapElement k : p' ->
            update (EMap $ Map.singleton k ty) (MkPath p') env
        [FunN fn] -> let tenv = unTyEnv env in
            case Map.lookup fn tenv of
                Just ty' -> MkTyEnv $ Map.insert fn (combine ty ty') tenv
                Nothing  -> MkTyEnv $ Map.insert fn ty tenv
        _ -> error "Internal error"


data AliasEnv = MkAliasEnv
    { aliasMap  :: IntMap ErlType
    , nextIndex :: !Int
    } deriving (Eq, Ord)

instance Show AliasEnv where
    show MkAliasEnv{..} =
        intercalate "\n" (map (\(i, t) -> "$" ++ show i ++ " -> " ++ show t) $ IntMap.toList aliasMap) ++
        "\n"

-- not good: throw error!
lookupAlias :: Int -> AliasEnv -> ErlType
lookupAlias i MkAliasEnv{..} = aliasMap IntMap.! i

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

aliases :: ErlType -> IntSet
aliases = para visit where
    visit :: ErlType -> [IntSet] -> IntSet
    visit (EAliasMeta i) is = IntSet.insert i $ mconcat is
    visit _ is              = mconcat is

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
        erase (EAliasMeta a') | a' `elem` as = EUnion HashSet.empty
        erase (EUnion ts) = mkFlatUnion $ HashSet.map erase ts
        erase t = t

-- -- | Change all occurences of aliases to newT in a type
substTy :: [Int] -> ErlType -> ErlType -> ErlType
substTy as newT = transform go where
    go (EAliasMeta alias) | alias `elem` as = newT
    go t = t

substTy' :: IntMap ErlType -> ErlType -> ErlType
substTy' sub = transform go where
    go (EAliasMeta alias) =
        case IntMap.lookup alias sub of
            Just t -> t
            _      -> EAliasMeta alias
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
aliasTuple conf t = postwalk conf t f where
 --   f conf' t'@(ENamedAtom _) = reg conf' t'
    f conf' t'@(ETuple (ENamedAtom _ : _)) = reg conf' t'
    f conf' (EUnion ts) | any (\case { EAliasMeta _ -> True; _ -> False}) ts =
        reg conf' (EUnion $ HashSet.map (resolve conf') ts)
    f conf' t' = (conf', t')

squash :: SquashConfig -> [Int] -> IntSet -> SquashConfig
squash conf []     _d = conf
squash conf@SquashConfig{..} (a1:w) d  = squash conf' (w ++ IntSet.toList as) (IntSet.insert a1 d) where
    as = aliases (lookupAlias a1 aliasEnv) IntSet.\\ d
    ap = IntSet.delete a1 d
    -- refers to ai, what is it???
    f delta a2 =
        if not (shouldMerge (map (resolve delta) [EAliasMeta a1, EAliasMeta a2]))
        then delta
        else mergeAliases delta [a1, a2] -- or include the whole worklist?

    conf' =
        if IntSet.member a1 d then conf else IntSet.foldl' f conf (ap <> as)

squashAll :: SquashConfig -> ErlType -> SquashConfig
squashAll conf0 t = confn where
    as = aliases t
    confn = IntSet.foldl' (\conf' i -> squash conf' [i] IntSet.empty) conf0 as

-- localGlobalSquash :: SquashConfig -> ErlType -> SquashConfig
-- localGlobalSquash conf0 t = confn where
--     as = aliasesInTyRec IntSet.empty t conf0
--     confn = _

squashLocal :: TyEnv -> SquashConfig
squashLocal tEnv = pruneAliases $ removeProxyAliases $ Map.foldlWithKey h initialConf (unTyEnv tEnv) where
    initialConf = SquashConfig (MkAliasEnv IntMap.empty 0) (MkTyEnv Map.empty)

    h conf n t = addFunction n t1 conf2 where
        (conf1, t1) = aliasTuple conf t
        conf2 = squashAll conf1 t1
        -- squash in unions as well

-- IMPORTANT! UPDATE WHEN ErlType changes
postwalk :: SquashConfig -> ErlType ->
             (SquashConfig -> ErlType -> (SquashConfig, ErlType)) ->
             (SquashConfig, ErlType)
postwalk conf_ t_ f = postwalk' conf_ t_ where
    postwalk' conf t = case t of
        ETuple ts ->
            let (conf', ts') = foldChildren ts in f conf' (ETuple ts')
        -- inefficient, is hashset the correct choice here?
        EUnion hs ->
            let (conf', hs') = foldChildren (HashSet.toList hs) in f conf' (EUnion $ HashSet.fromList hs')
        EFun args ret ->
            let (conf', args') = foldChildren args
                (conf'', ret') = postwalk' conf' ret
            in
                f conf'' (EFun args' ret')
        EMap m ->
            -- TODO: handle maps
            (conf, EMap m)
        EContainer c -> doCont c
        _ -> f conf t
        where
        foldChildren = foldr go (conf, mempty)
        go t' (conf', ts') =
            let (conf'', t'') = postwalk' conf' t' in
            (conf'', t'':ts')

        doCont (CList t1) =
            let (conf', t1') = postwalk' conf t1 in f conf' (EContainer $ CList t1')
        doCont (CDict t1) =
            let (conf', t1') = postwalk' conf t1 in f conf' (EContainer $ CDict t1')
        doCont (COldSet t1) =
            let (conf', t1') = postwalk' conf t1 in f conf' (EContainer $ COldSet t1')
        doCont (CGbSet t1) =
            let (conf', t1') = postwalk' conf t1 in f conf' (EContainer $ CGbSet t1')
        doCont (CArray t1) =
            let (conf', t1') = postwalk' conf t1 in f conf' (EContainer $ CArray t1')
        doCont CGb = f conf (EContainer CGb)
        doCont (CGbTree t1 t2) =
            let (conf', t1') = postwalk' conf t1
                (conf'', t2') = postwalk' conf' t2
            in
                f conf'' (EContainer $ CGbTree t1' t2')

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
        visit (EUnion ts) = EUnion $ HashSet.foldl' combineUnion HashSet.empty ts
        visit t           = t


-- | Remove "proxy" aliases like $1 in: $1 -> $2 -> {'rec', integer()}
removeProxyAliases :: SquashConfig -> SquashConfig
removeProxyAliases conf@SquashConfig{aliasEnv = MkAliasEnv{..}, tyEnv = MkTyEnv funs} =
    conf {aliasEnv = MkAliasEnv newAliasMap nextIndex, tyEnv = MkTyEnv newFuns} where
        newAliasMap = IntMap.map remove aliasMap
        newFuns = Map.map remove funs

        remove = transform resolveProxy

        resolveProxy (EUnion ts) | HashSet.size ts == 1 = resolveProxy (head $ HashSet.toList ts)
        resolveProxy (EUnion ts) = EUnion $ HashSet.foldl' go HashSet.empty ts
        resolveProxy (EAliasMeta i) =
            case lookupAlias i (aliasEnv conf) of
                EAliasMeta j -> resolveProxy (EAliasMeta j)
                _            -> EAliasMeta i
        resolveProxy  t = t

        go set (EUnion ts) = ts <> set
        go set t           = HashSet.insert t set

-- | Remove mappings for unused aliases.
pruneAliases :: SquashConfig -> SquashConfig
pruneAliases conf@SquashConfig{aliasEnv = MkAliasEnv{..}} =
    conf {aliasEnv = MkAliasEnv newAliasMap nextIndex } where
    usedAliases = getReachable conf
    newAliasMap = IntMap.restrictKeys aliasMap usedAliases

aliasesToGraph :: IntMap ErlType -> AdjacencyIntMap
aliasesToGraph aliasMap =
    Algebra.Graph.AdjacencyIntMap.edges $ IntMap.toList aliasMap >>= uncurry getEdges where

    --maybe we don't need this, just overlay edges and return an AdjacencyIntMap
    getEdges :: Int -> ErlType -> [(Int, Int)]
    getEdges from = para visit where
        visit :: ErlType -> [[(Int, Int)] ] -> [(Int, Int)]
        visit (EAliasMeta i) es = (from, i) : concat es
        visit _ is              = concat is

getReachable :: SquashConfig -> IntSet
getReachable SquashConfig{aliasEnv=MkAliasEnv aliasMap _, tyEnv=MkTyEnv funs} = reachable where
    aliasGraph = aliasesToGraph aliasMap
    immediate = Map.elems funs >>= aliases'
    reachable = IntSet.fromList $ immediate ++ concat (bfs aliasGraph immediate)

    aliases' :: ErlType -> [Int]
    aliases' = para visit where
        visit :: ErlType -> [[Int]] -> [Int]
        visit (EAliasMeta i) is = i:concat is
        visit _ is              = concat is

-- | Inline aliases that have only one reference to them.
-- todo: this is not good enough, circular substituting references should be avoided!
-- todo: some should be inlined, but they are not! why??
inlineAliases :: SquashConfig -> SquashConfig
inlineAliases conf@SquashConfig{aliasEnv = ae@MkAliasEnv{..},tyEnv = MkTyEnv funs} =
    conf {aliasEnv = MkAliasEnv newAliasMap nextIndex, tyEnv = MkTyEnv newTyEnv } where

    refsInFuns = IntMap.unionsWith (+) (map numOfRefs $ Map.elems funs)
    refsInAliases = IntMap.unionsWith (+) (map numOfRefs $ IntMap.elems aliasMap)

    singleRefs = IntMap.filter (==1) $ IntMap.unionWith (+) refsInFuns refsInAliases

    -- the problem with this is:
    -- inline (1375,{'clauses', list($1374)})
    -- _then_
    -- with this it is back: (1376,{'fun', {integer(), integer()}, $1375})
    sub = IntMap.mapWithKey (\a _ -> lookupAlias a ae) singleRefs

    -- do not substitute into itself
    newAliasMap =
        IntMap.mapWithKey (\a t -> substTy' (IntMap.delete a sub) t) aliasMap
    newTyEnv = Map.map (substTy' sub) funs

   -- usedAliases = IntSet.unions $ map (\t -> aliasesInTyRec IntSet.empty t conf) tys
   -- newAliasMap = IntMap.restrictKeys aliasMap usedAliases

numOfRefs :: ErlType -> IntMap Int
numOfRefs = para visit where
    visit (EAliasMeta i) ims =
        IntMap.insertWith (+) i 1 (IntMap.unionsWith (+) ims)
    visit _ ims =
        IntMap.unionsWith (+) ims

-- -------------------------------------------------------------------------------
-- -- Global squashing
-- -------------------------------------------------------------------------------

-- originally conf' was not used, which is weird
singleRecFun :: SquashConfig -> FunName -> ErlType -> SquashConfig
singleRecFun conf fn t = addFunction fn t' conf' where
    (conf', t') = postwalk conf t f

    --f co ty@(ENamedAtom _) = reg co ty
    f co ty@(ETuple (ENamedAtom _ : _)) = reg co ty
    f co ty                             = (co, ty)

singleRecAlias :: SquashConfig -> ErlType -> (SquashConfig, ErlType)
singleRecAlias conf t = (conf', t') where
    (conf', t') = postwalk conf t f

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
    foldChildren ts confN = (confN', reverse ts') where
        (confN', ts') = foldl' visit (confN, []) ts

        visit (c, acc) ty =
            let (c1, ty1) = singleRecAlias c ty in
            (c1, ty1 : acc)

data Tag = RecordTag Text Int | SingleAtom Text
    deriving (Eq, Ord, Show)

-- we might even not need the EUnion case
-- because same tag, different union elements are handled already in the union creation
tag :: SquashConfig -> ErlType -> Maybe Tag
tag conf ty = case resolve conf ty of
    (ETuple (ENamedAtom txt : ts)) -> Just $ RecordTag txt $ length ts
    ENamedAtom txt                 -> Just $ SingleAtom txt
--    EUnion ts -> sameTagged $ Set.toList ts
    _                              -> Nothing
    -- where
    --     sameTagged :: [ErlType] -> Maybe Tag
    --     sameTagged [] = Nothing
    --     sameTagged [t] = tag conf t
    --     sameTagged (t:ts) = case sameTagged ts of
    --         Just tag' | tag conf t == Just tag' -> Just tag'
    --         _ -> Nothing

groupSimilarRecs :: SquashConfig -> Map Tag [Int]
groupSimilarRecs conf@SquashConfig{aliasEnv=MkAliasEnv aliasMap _} =
    IntMap.foldlWithKey visit Map.empty aliasMap where

    visit acc key _ =
        case tag conf (EAliasMeta key) of
            Just theTag ->
                Map.insertWith (++) theTag [key] acc
             --   Map.alter (\case Just is -> Just (key : is); _ -> Just [key]) theTag acc
            _ -> acc

-- squashUnionElements :: Set ErlType -> Set ErlType
-- squashUnionElements ts = unTaggedSet <> Set.fromList (Map.elems tagMap) where
--     (unTaggedSet, tagMap) = Set.foldl visit (Set.empty, Map.empty) ts

--     visit (unTagged, tagged) t =
--         case immediateTag t of
--             Just theTag ->
--                 (unTagged, Map.alter (\case Just t' -> Just $ t `combine` t'; _ -> Just t) theTag tagged)
--             _ -> (Set.insert t unTagged, tagged)

--     immediateTag :: ErlType -> Maybe Tag
--     immediateTag t = case t of
--         (ETuple (ENamedAtom txt : rest)) -> Just $ RecordTag txt $ length rest
--         ENamedAtom txt                   -> Just $ SingleAtom txt
--         _                                -> Nothing

squashHorizontally :: SquashConfig -> SquashConfig
squashHorizontally conf =
    Map.foldl' mergeAliases conf (groupSimilarRecs conf)

squashHorizontallyMulti :: SquashConfig -> SquashConfig
squashHorizontallyMulti conf =
    foldl' mergeAliases conf (getEq (aliasesToTags conf) conf)

tagMulti :: SquashConfig -> ErlType -> Set Tag
tagMulti conf ty = case resolve conf ty of
    (ETuple (ENamedAtom txt : ts)) -> Set.singleton $ RecordTag txt $ length ts
    ENamedAtom txt                 -> Set.singleton $ SingleAtom txt
    EUnion ts                      -> Set.unions $ map (tagMulti conf) $ HashSet.toList ts
    _                              -> Set.empty

-- groupSimilarRecsMulti :: SquashConfig -> Map (Set Tag) [Int]
-- groupSimilarRecsMulti conf@SquashConfig{aliasEnv=MkAliasEnv aliasMap _} =
--     Map.map Data.List.nub groups where

--     groups = IntMap.foldlWithKey visit Map.empty aliasMap

--     visit acc key _ = let tags = tagMulti conf (EAliasMeta key) in
--         if not (Set.null tags)
--         then Map.alter (\case Just is -> Just (key : is); _ -> Just [key]) tags acc
--         else acc

--type TagEq s a = Equiv.EquivM s (Set Tag) Int a

-- Might be better if the tags were precomputed?
-- also, what happens with the Set.null tagged aliases?
--- runEq :: SquashConfig -> (forall s. TagEq s a) -> a
--runEq conf = Equiv.runEquivM (tagMulti conf . EAliasMeta) Set.union

---getEq :: Map Tag [Int] -> SquashConfig -> [[Int]]
getEq :: Map Tag [Int] -> SquashConfig -> [[Int]]
getEq tagMap conf = runIdentity $ STT.runSTT $ do
    st <- Equiv.leastEquiv (tagMulti conf . EAliasMeta) Set.union
    mapM_ (visit st) tagMap
    clss <- Equiv.classes st
    traverse (classesToAliases st) clss where
        visit = Equiv.equateAll

        classesToAliases st cl = do
            tags <- Equiv.desc st cl
            let l = foldl' (\as tg -> Map.findWithDefault [] tg tagMap ++ as) [] tags
            return $ nubInt l


aliasesToTags :: SquashConfig -> Map Tag [Int]
aliasesToTags conf@SquashConfig{aliasEnv=MkAliasEnv aliasMap _} =
    Map.map nubInt groups where

    groups = IntMap.foldlWithKey visit Map.empty aliasMap

    visit acc alias _ =
        Set.fold (addTag alias) acc (tagMulti conf (EAliasMeta alias))

    addTag alias tg = Map.insertWith (++) tg [alias]

-- Clean up the multiple uses of removeSingleUnions, why do we need multiples of them?
-- Could we unify proxy removal, pruning, etc?
squashGlobal :: SquashConfig -> SquashConfig
squashGlobal = compose [ aliasSingleRec
                       -- horizontal squash, single
                       , squashHorizontally
                       , removeProxyAliases
                       , pruneAliases
                       -- horizontal squash, multi
                       , squashHorizontallyMulti
                       , removeProxyAliases
                       , pruneAliases
                       , inlineAliases
                       , pruneAliases
                       , tryRemoveUnknowns
                       ]
-- foldl'?
compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id
