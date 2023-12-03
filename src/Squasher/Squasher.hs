{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Squasher.Squasher(runner) where

import           Data.ByteString.Lazy                    (ByteString)
import           Data.IntMap.Strict                      (IntMap)
import           Data.IntSet                             (IntSet)
import qualified Data.IntSet                             as IntSet
import           Data.List                               (foldl', partition)
import qualified Data.Map.Strict                         as Map
--import Data.Generics.Uniplate.Operations (transformM)
import           Algebra.Graph.AdjacencyIntMap           (AdjacencyIntMap)
import qualified Algebra.Graph.AdjacencyIntMap
import           Algebra.Graph.AdjacencyIntMap.Algorithm (bfs)
import           Control.Monad                           (foldM, guard,
                                                          zipWithM)
import           Control.Monad.Trans.Except              (Except, throwE)
import           Data.Generics.Uniplate.Data
import qualified Data.HashSet                            as HashSet
import qualified Data.IntMap.Strict                      as IntMap
import           Data.Maybe                              (fromMaybe)
import           Debug.Trace

import           Foreign.Erlang.Term
import           Squasher.Common
import           Squasher.Global
import           Squasher.Local
import           Squasher.Types


runner :: Options -> [Term] -> Except String (TyEnv, SquashConfig, SquashConfig)
runner opts terms = do
    entries <- mapM entryFromTerm terms
    let env = foldl' (\tenv (t, p) -> update t p tenv) (MkTyEnv Map.empty) entries
    let global = 
            case strategy opts of
                S1 -> squashGlobal1
                S2 -> squashGlobal2
                S3 -> squashGlobal3
    let env' = pruneAliases $ removeProxyAliases $ squashLocal opts env
    return $ (env, env', post $ global env')

squashGlobal1, squashGlobal2, squashGlobal3, post :: SquashConfig -> SquashConfig
squashGlobal1 = compose [ aliasSingleRec
                        -- horizontal squash, single
                        , squash
                        , removeProxyAliases
                        , pruneAliases
                        , squashMulti
                        -- horizontal squash, multi
                        ]
squashGlobal2 = compose [ aliasSingleRec
                       -- horizontal squash, single
                        , strictSquashMulti
                       -- , removeProxyAliases
                       -- , pruneAliases
                       -- , strictSquashMulti
                        ]
squashGlobal3 = compose [ aliasSingleRec
                        , strictestSquashMulti
                        ]
post = compose [ removeProxyAliases
               , pruneAliases
               , inlineAliases
               , tryRemoveUnknowns
               , squashTuples
               , removeSubsets
               , upcastAtomUnions
               , inlineAliases
               , tryRemoveUnknowns
               , inlineAliases
               , pruneAliases
               ]

-- foldl'?
compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

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
        visit (EUnion ts) = mkUnion $ HashSet.foldl' combineUnion HashSet.empty ts
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
            case lookupAlias i conf of
                EAliasMeta j -> resolveProxy (EAliasMeta j)
                EUnion{}     -> EAliasMeta i
                ETuple{}     -> EAliasMeta i
                EContainer{} -> EAliasMeta i
                EFun{}       -> EAliasMeta i
                t            -> t
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
-- TODO: use the graphs
inlineAliases :: SquashConfig -> SquashConfig
inlineAliases conf@SquashConfig{aliasEnv = MkAliasEnv{..},tyEnv = MkTyEnv funs} =
    conf {aliasEnv = MkAliasEnv newAliasMap nextIndex, tyEnv = MkTyEnv newTyEnv } where

    refsInFuns = IntMap.unionsWith (+) (map numOfRefs $ Map.elems funs)
    refsInAliases = IntMap.unionsWith (+) (map numOfRefs $ IntMap.elems aliasMap)

    singleRefs = IntMap.filter (<=1) $ IntMap.unionWith (+) refsInFuns refsInAliases

    -- the problem with this is:
    -- inline (1375,{'clauses', list($1374)})
    -- _then_
    -- with this it is back: (1376,{'fun', {integer(), integer()}, $1375})
    sub = IntMap.mapWithKey (\a _ -> lookupAlias a conf) singleRefs

    -- do not substitute into itself
    newAliasMap =
        IntMap.mapWithKey (\a t -> substTy' (IntMap.delete a sub) t) aliasMap
    newTyEnv = Map.map (substTy' sub) funs

squashTuples :: SquashConfig -> SquashConfig
squashTuples conf@SquashConfig{aliasEnv = MkAliasEnv{..},tyEnv = MkTyEnv funs, options=Options{..}} =
    conf {aliasEnv = MkAliasEnv newAliasMap nextIndex, tyEnv = MkTyEnv newFuns} where

    newAliasMap = IntMap.map equateElements aliasMap
    newFuns = Map.map equateElements funs

    equateElements :: ErlType -> ErlType
    equateElements = transform visit

    visit :: ErlType -> ErlType
    visit (EUnion ts) = fromMaybe (EUnion ts) $ squashUnions $ HashSet.toList ts
    visit t           = t

    squashUnions :: [ErlType] -> Maybe ErlType
    squashUnions ts = do
        let (tuples, other) = partition isTuple ts
        guard $ length tuples > tupleUnionSize
        guard $ null other || upcastMixedTuples
        tuples' <- foldM f EUnknown tuples
        return $ mkUnion $ HashSet.fromList $ tuples' : other

    isTuple EUnknown   = True
    isTuple (ETuple _) = True
    isTuple _          = False

    f t        EUnknown = Just t
    f EUnknown t        = Just t
    f (ETuple (t1:ts1)) (ETuple (t2:ts2)) | length ts1 == length ts2 = do
        ts' <- zipWithM (\t1' t2' -> equate (resolve conf t1') (resolve conf t2')) ts1 ts2
        return $ ETuple (combine t1 t2:ts')
    f _                 _                 = Nothing

removeSubsets :: SquashConfig -> SquashConfig
removeSubsets conf@SquashConfig{aliasEnv = MkAliasEnv{..},tyEnv = MkTyEnv funs} =
    conf {aliasEnv = MkAliasEnv newAliasMap nextIndex, tyEnv = MkTyEnv newFuns} where

    newAliasMap = IntMap.map doElements aliasMap
    newFuns = Map.map doElements funs

    doElements :: ErlType -> ErlType
    doElements = transform visit

    visit :: ErlType -> ErlType
    visit (EUnion ts) = mkUnion $ doUnion ts
    visit t           = t

    --doUnion :: HashSet ErlType -> HashSet ErlType
    doUnion ts = HashSet.difference ts $ HashSet.unions $ map getAliased (HashSet.toList ts)

    -- we could make this more recursive
    getAliased (EAliasMeta i) = case resolve conf (EAliasMeta i) of
        EUnion ts -> ts
        t         -> HashSet.singleton t
    getAliased _ = HashSet.empty

upcastAtomUnions :: SquashConfig -> SquashConfig
upcastAtomUnions conf@SquashConfig{aliasEnv = MkAliasEnv{..}, tyEnv = MkTyEnv funs, options=Options{..}} =
    conf {aliasEnv = MkAliasEnv newAliasMap nextIndex, tyEnv = MkTyEnv newFuns} where
        newAliasMap = IntMap.map equateElements aliasMap
        newFuns = Map.map equateElements funs

        equateElements :: ErlType -> ErlType
        equateElements = transform visit

        visit :: ErlType -> ErlType
        visit (EUnion ts) =
            let (atoms, other) = partition isAtomLike $ HashSet.toList ts in
            -- maybe make this switchable, so mixed unions don't get upcast
            if length atoms > atomUnionSize then
                if null other then
                    EAnyAtom
                else if upcastMixedAtoms then
                    EUnion $ HashSet.fromList $ EAnyAtom:other
                else
                    EUnion ts
            else
                EUnion ts
        visit t = t

        -- this wont' be a loop because we ensured no loops in mergeAliases
        -- can inlineAliases or something else mess it up?
        isAtomLike t = case resolve conf t of
            EAnyAtom     -> True;
            ENamedAtom _ -> True
            EBoolean     -> True
            EUnknown     -> True
            EUnion ts    -> all isAtomLike ts
            _            -> False

numOfRefs :: ErlType -> IntMap Int
numOfRefs = para visit where
    visit (EAliasMeta i) ims =
        IntMap.insertWith (+) i 1 (IntMap.unionsWith (+) ims)
    visit _ ims =
        IntMap.unionsWith (+) ims