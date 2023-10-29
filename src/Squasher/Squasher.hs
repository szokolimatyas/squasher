{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Squasher.Squasher(runner) where

import           Data.ByteString.Lazy                    (ByteString)
import           Data.IntMap.Strict                      (IntMap)
import           Data.IntSet                             (IntSet)
import qualified Data.IntSet                             as IntSet
import qualified Data.Map.Strict                         as Map
import           Squasher.Types
--import Data.Generics.Uniplate.Operations (transformM)
import           Algebra.Graph.AdjacencyIntMap           (AdjacencyIntMap)
import qualified Algebra.Graph.AdjacencyIntMap
import           Algebra.Graph.AdjacencyIntMap.Algorithm (bfs)
import           Control.Monad.Trans.Except              (Except,
                                                          throwE)
import           Data.Binary                             (decodeOrFail)
import           Data.Binary.Get                         (ByteOffset)
import           Data.Generics.Uniplate.Data
import qualified Data.HashSet                            as HashSet
import qualified Data.IntMap.Strict                      as IntMap
import           Debug.Trace
import           Foreign.Erlang.Term

import Squasher.Common
import Squasher.Local
import Squasher.Global


runner :: ByteString -> Except String SquashConfig
runner bs = case dec of
    Right (_, _, MkExternalTerm (List terms Nil)) -> do
        entries <- mapM entryFromTerm terms
        traceM "Start update!"
        let env = foldl (\tenv (t, p) -> update t p tenv) (MkTyEnv Map.empty) entries
       -- let env' = MkTyEnv (Map.take 20 $ unTyEnv env) --MkTyEnv (Map.take 1 $ unTyEnv env) --MkTyEnv (Map.take 1 $ Map.drop 14 (unTyEnv env))
        let env' = env
        traceM $ "Env:\n" ++ show (Map.size $ unTyEnv env')
        let env'' = Debug.Trace.trace "Local squash done, start global squash!" $ pruneAliases $ removeProxyAliases $ squashLocal env'
        let res = squashGlobal env''
        return res
    Right (_, _, MkExternalTerm terms) -> throwE $ "Terms are in a wrong format: " ++ show terms
    Left (_, _, str) -> throwE $ "Could not parse bytestring, error: " ++ str
  where
    dec ::  Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, ExternalTerm)
    dec = decodeOrFail bs


-- Clean up the multiple uses of removeSingleUnions, why do we need multiples of them?
-- Could we unify proxy removal, pruning, etc?
squashGlobal :: SquashConfig -> SquashConfig
squashGlobal = compose [ aliasSingleRec
                       -- horizontal squash, single
                       , strictSquashHorizontally
                       , removeProxyAliases
                       , pruneAliases
                       , strictSquash
                       , removeProxyAliases
                       , pruneAliases
                       , removeProxyAliases
                       , pruneAliases
                       , inlineAliases
                       , pruneAliases
                       , tryRemoveUnknowns
                       ]
-- squashGlobal = compose [ aliasSingleRec
--                        -- horizontal squash, single
--                        , squashHorizontally
--                        , removeProxyAliases
--                        , pruneAliases
--                        -- horizontal squash, multi
--                        , squashHorizontallyMulti
--                        , removeProxyAliases
--                        , pruneAliases
--                        , inlineAliases
--                        , pruneAliases
--                        , tryRemoveUnknowns
--                        ]
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
            case lookupAlias i conf of
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
    sub = IntMap.mapWithKey (\a _ -> lookupAlias a conf) singleRefs

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