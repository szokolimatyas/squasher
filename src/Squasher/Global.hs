{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Squasher.Global(squashHorizontally, squashHorizontallyMulti, aliasSingleRec, strictSquash) where

import qualified Data.HashSet                            as HashSet
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import qualified Data.IntMap.Strict                      as IntMap
import           Data.IntMap.Strict                      (IntMap)
import qualified Data.IntSet                             as IntSet
import           Data.Text                               (Text)
import           Data.Set                                (Set)
import qualified Data.Set                                as Set
import           Data.Containers.ListUtils               (nubInt, nubOrd)
import           Data.Foldable                           (foldl')
import           Data.Functor.Identity                   (runIdentity)
import qualified Data.Equivalence.STT                    as Equiv
import qualified Control.Monad.ST.Trans                  as STT
import           Data.Maybe                              (mapMaybe, isJust)
import qualified Debug.Trace
import           Algebra.Graph.AdjacencyIntMap           (AdjacencyIntMap)
import qualified Algebra.Graph.AdjacencyIntMap
import qualified Algebra.Graph.AdjacencyIntMap.Algorithm

import           Squasher.Common
import           Squasher.Types


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


groupSimilarRecs :: SquashConfig -> Map Tag [Int]
groupSimilarRecs conf@SquashConfig{aliasEnv=MkAliasEnv aliasM _} =
    IntMap.foldlWithKey visit Map.empty aliasM where

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
aliasesToTags conf@SquashConfig{aliasEnv=MkAliasEnv aliasM _} = Map.map nubInt groups where
    groups = IntMap.foldlWithKey visit Map.empty aliasM

    visit acc alias _ =
        Set.fold (addTag alias) acc (tagMulti conf (EAliasMeta alias))

    addTag alias tg = Map.insertWith (++) tg [alias]

-------------------------------------------------------------------------------
-- LESS AGRESSIVE SQUASHING
-------------------------------------------------------------------------------

strictSquash :: SquashConfig -> SquashConfig
strictSquash conf = iter maps conf' where
    groups = aliasesToGroups conf
    (conf', maps) = squashSameTagsInUnion conf groups

    iter []     confN = confN
    iter (m:ms) confN = iter ms (oneIteration m ms confN)

    oneIteration :: Map Tag Int -> [Map Tag Int] -> SquashConfig -> SquashConfig
    oneIteration m ms confN = mergeAliases confN $ Map.elems m ++ concatMap Map.elems toMerge where
        toMerge = filter f ms

        -- merge two groups if:
        -- - there are same tagged types
        -- - all of the matching same tagged type have similar fields
        f m' = 
            let inter = Map.intersectionWith (,) m m' in
            Map.size inter > 0 -- && all typesAreSimilar inter

        typesAreSimilar (i1, i2) = case (lookupAlias i1 conf, lookupAlias i2 conf) of
            (ETuple ts1, ETuple ts2) -> 
                -- the number of matching equatable fields are greater than number of all fields divided by two
                count (\(t1, t2) -> isJust $ equate (resolve conf t1) (resolve conf t2)) (zip ts1 ts2) > div (length ts1) 2
            (ENamedAtom txt1, ENamedAtom txt2) -> txt1 == txt2
            _ -> False

    -- iterate over list of maps, if two maps have a nonempty intersection, and:
    -- the intersection records have "enough" equatable fields

count :: (a -> Bool) -> [a] -> Int
count f = foldl' (\acc a -> if f a then acc + 1 else acc) 0

squashSameTagsInUnion :: SquashConfig -> [[(Tag, Int)]] -> (SquashConfig, [Map Tag Int])
squashSameTagsInUnion conf = foldl' go (conf, []) where
    go (conf', ms) ps = 
        let (conf'', m) = foldl' add (conf', Map.empty) ps in
            (conf'', m:ms)
    
    add (conf', m) (tg, a) = case Map.lookup tg m of
        Just a' -> (mergeAliases conf' [a', a], m)
        Nothing -> (conf', Map.insert tg a m)

-- all toplevel parts of a union
-- returns a list, because the same tag could be applied to different types
aliasesToGroups :: SquashConfig -> [[(Tag, Int)]]
aliasesToGroups conf@SquashConfig{aliasEnv=(MkAliasEnv aliasM _)} = groups where
   
    graph = aliasesToGraph aliasM

    groups = IntMap.foldlWithKey visit [] aliasM

    visit :: [[(Tag, Int)]] -> Int -> ErlType -> [[(Tag, Int)]]
    visit acc alias _ = nubOrd (mapMaybe (\i -> (,i) <$> tag conf (EAliasMeta i)) $ topAliases alias) : acc

    topAliases :: Int -> [Int]
    topAliases = Algebra.Graph.AdjacencyIntMap.Algorithm.reachable graph


aliasesToGraph :: IntMap ErlType -> AdjacencyIntMap
aliasesToGraph aliasM =
    Algebra.Graph.AdjacencyIntMap.edges $ IntMap.toList aliasM >>= uncurry getEdges where

    getEdges :: Int -> ErlType -> [(Int, Int)]
    getEdges from (EAliasMeta i) = [(from, i)] 
    getEdges from (EUnion ts)    = concatMap (getEdges from) ts
    getEdges _    _              = []
