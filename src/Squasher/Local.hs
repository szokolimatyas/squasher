{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Squasher.Local(squashLocal) where

import qualified Data.HashSet       as HashSet
import qualified Data.IntMap.Strict as IntMap
import           Data.IntSet        (IntSet)
import qualified Data.IntSet        as IntSet
import qualified Data.Map.Strict    as Map
import           Squasher.Common
import           Squasher.Types
import qualified Debug.Trace

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

aliasTuple :: SquashConfig -> ErlType -> (SquashConfig, ErlType)
aliasTuple conf t = postwalk conf t f where
 --   f conf' t'@(ENamedAtom _) = reg conf' t'
    f conf' t'@(ETuple (ENamedAtom _ : _)) = reg conf' t'
    f conf' (EUnion ts) | any (\case { EAliasMeta _ -> True; _ -> False}) ts =
        reg conf' (EUnion $ HashSet.map (resolve conf') ts)
    f conf' t' = (conf', t')

squash :: SquashConfig -> [Int] -> IntSet -> SquashConfig
squash conf []     _d = conf
squash conf (a1:w) d  = squash conf' (w ++ IntSet.toList as) (IntSet.insert a1 d) where
    as = aliases (lookupAlias a1 conf) IntSet.\\ d
    ap = Debug.Trace.trace ((show a1) ++ " " ++ (show $ (IntSet.delete a1 d) <> as)) $ IntSet.delete a1 d
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

squashLocal :: Options -> TyEnv -> SquashConfig
squashLocal opts tEnv = Map.foldlWithKey h initialConf (unTyEnv tEnv) where
    initialConf = SquashConfig (MkAliasEnv IntMap.empty 0) (MkTyEnv Map.empty) opts

    h conf n t = addFunction n t1 conf2 where
        (conf1, t1) = Debug.Trace.trace (show $ aliasMap $ aliasEnv $ fst $ aliasTuple conf t) aliasTuple conf t
        conf2 = squashAll conf1 t1
