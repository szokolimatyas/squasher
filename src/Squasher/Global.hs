{-# LANGUAGE RankNTypes        #-}
module Squasher.Global(squashHorizontally, squashHorizontallyMulti, aliasSingleRec) where

import qualified Data.HashSet                            as HashSet
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import qualified Data.IntMap.Strict                      as IntMap
import           Data.Text                               (Text)
import           Data.Set                                (Set)
import qualified Data.Set                                as Set
import           Data.Containers.ListUtils               (nubInt)
import           Data.Foldable                           (foldl')
import           Data.Functor.Identity                   (runIdentity)
import qualified Data.Equivalence.STT                    as Equiv
import qualified Control.Monad.ST.Trans                  as STT

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
    -- where
    --     sameTagged :: [ErlType] -> Maybe Tag
    --     sameTagged [] = Nothing
    --     sameTagged [t] = tag conf t
    --     sameTagged (t:ts) = case sameTagged ts of
    --         Just tag' | tag conf t == Just tag' -> Just tag'
    --         _ -> Nothing

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
aliasesToTags conf@SquashConfig{aliasEnv=MkAliasEnv aliasM _} =
    Map.map nubInt groups where

    groups = IntMap.foldlWithKey visit Map.empty aliasM

    visit acc alias _ =
        Set.fold (addTag alias) acc (tagMulti conf (EAliasMeta alias))

    addTag alias tg = Map.insertWith (++) tg [alias]

