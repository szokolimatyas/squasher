{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Squasher.Naming(Name(..), nameAll, paramNames, emptyParamNames, initialNames) where

import qualified Data.HashSet    as HashSet
import           Data.IntMap     (IntMap)
import qualified Data.IntMap     as IntMap
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Squasher.Common
import           Squasher.Types
import qualified Foreign.Erlang.Term as Erlang
import           Data.Maybe      (mapMaybe)
import           Data.Foldable   (foldl')

data Name = Record Text
          | Alias Text
          deriving (Eq, Ord, Show)

nameAll :: IntMap Text -> SquashConfig -> IntMap Name
nameAll namesFromParams SquashConfig{aliasEnv=MkAliasEnv{..}, options=Options{..}} = IntMap.foldlWithKey nameAlias initialNames aliasMap where
    nameAlias :: IntMap Name -> Int -> ErlType -> IntMap Name
    nameAlias names i t = case t of
        (ETuple (ENamedAtom txt : ts)) | length ts >= recordSize ->
            insertNew i (Record txt) names
        (ETuple (ENamedAtom txt : _)) ->
            insertNew i (Alias txt) names
        (ETuple (EUnion uni : _)) | Just ns <- mapM getTag $ HashSet.toList uni ->
            insertNew' i (fitName t $ fitNames ns <> "_tuple") names
        (EUnion uni) | Just ns <- mapM getTag $ HashSet.toList uni ->
            insertNew' i (fitName t $ fitNames ns) names
        (EUnion uni) ->
            let ns = map simpleName $ HashSet.toList uni in
            insertNew' i (fitName t $ fitNames ns) names
        _ ->
            insertNew' i (simpleName t) names

    insertNew :: Int -> Name -> IntMap Name -> IntMap Name
    insertNew i (Record txt) m =
        if Record txt `elem` m then
            insertNew' i txt m
        else
            IntMap.insert i (Record txt) m
    insertNew i (Alias txt) m =
        if Alias txt `elem` m then
            IntMap.insert i (Alias $ txt <> Text.pack (show i)) m
        else
            IntMap.insert i (Alias txt) m

    insertNew' :: Int -> Text -> IntMap Name -> IntMap Name
    insertNew' i txt m = 
        let n = IntMap.findWithDefault txt i namesFromParams in
        if Alias n `elem` m then
            IntMap.insert i (Alias $ n <> Text.pack (show i)) m
        else
            IntMap.insert i (Alias n) m


initialNames :: IntMap Name
initialNames = IntMap.fromList $ zip [-1, -2..] $ map Alias
    [ "any", "none", "dynamic", "pid", "port", "reference", "atom",  "float", "fun", "integer", "list", "map", "tuple", "function", "binary", "bitstring"]

fitNames :: [Text] -> Text
fitNames ts = Text.intercalate "_" $
    if sum (map Text.length ts) > 20 then
        map (Text.take 3) ts
    else
        ts

fitName :: ErlType -> Text -> Text
fitName _ txt | Text.length txt <= 30 = txt
fitName t _ = simpleName t
    -- handle {foo|bar|zag, _} --> foo_bar_zag
    -- handle {foo, _} | bar | {zag, _] --> foo_bar_zag
    -- handle integer() | {foo, _} | bar --> int_or_foo_or_bar / int_foo_bar
    -- handle $1 | $2 --> nameof $1 _ nameof $2  --> receive_discard/receive_or_discard
    -- handle {atom(), _} --> tuple1
getTag :: ErlType -> Maybe Text
getTag (ENamedAtom n)                = Just n
getTag EBoolean                      = Just "bool"
getTag (ETuple (ENamedAtom txt : _)) = Just txt
getTag _                             = Nothing

simpleName :: ErlType -> Text
simpleName ty = case ty of
    EInt                        -> "int"
    EFloat                      -> "float"
    -- escape the '-s?
    ENamedAtom a                -> a
    EAnyAtom                    -> "atom"
    ETuple (ENamedAtom txt : _) -> txt
    ETuple _                    -> "tuple"
    EAny                        -> "any"
    ENone                       -> "none"
    EUnion _                    -> "union"
    EFun _ _                    -> "function"
    EAliasMeta _                -> "var"
    EUnknown                    -> "unknown"
    EBinary                     -> "bin"
    EBitString                  -> "bitstring"
    EPid                        -> "pid"
    EPort                       -> "port"
    ERef                        -> "ref"
    EBoolean                    -> "bool"
    EMap _                      -> "map"
    EContainer c                -> simpleContName c

simpleContName :: Container a -> Text
simpleContName c = case c of
    CList _     -> "list"
    CDict _     -> "dict"
    COldSet _   -> "set"
    CGbSet _    -> "set"
    CGbTree _ _ -> "tree"
    CGb         -> "gb"
    CArray _    -> "array"

paramsFromTerm :: Erlang.Term -> [(FunName, [[Maybe Text]])]
paramsFromTerm (Erlang.List ts Erlang.Nil) = mapMaybe go ts where
    go (Erlang.Tuple [Erlang.Tuple [Erlang.Atom _ n, Erlang.Integer i], Erlang.List clauses Erlang.Nil]) =
        Just (MkFunName n (fromIntegral i), doClauses clauses)
    go _ = Nothing

    doClauses = mapMaybe doClause 
    doClause (Erlang.List ts' Erlang.Nil) = Just $ map doName ts'
    doClause _ = Nothing
    
    doName (Erlang.Tuple [Erlang.Atom _ "name", Erlang.Atom _ txt]) | Text.length txt > 1 =
        Just (Text.toLower txt)
    doName _ = Nothing
paramsFromTerm _ = []

emptyParamNames :: IntMap Text
emptyParamNames = IntMap.empty

paramNames :: SquashConfig -> Erlang.Term -> IntMap Text
paramNames SquashConfig{tyEnv=MkTyEnv tenv} term = IntMap.mapMaybe findMaxName namings where

    findMaxName :: Map Text Int -> Maybe Text
    findMaxName m = fst $ Map.foldlWithKey selMax (Nothing, 0) m where
        selMax (Nothing, _) txt i = (Just txt, i) 
        selMax (Just txt', i') txt i = if i >= i' then (Just txt, i) else (Just txt', i')

    -- (aliasId :-> (paramName :-> occurrenceNumber))
    namings :: IntMap (Map Text Int)
    namings = foldl' go IntMap.empty (paramsFromTerm term)

    go :: IntMap (Map Text Int) -> (FunName, [[Maybe Text]]) -> IntMap (Map Text Int)
    go acc (funName, lists) = foldl' zipp acc lists where

        zipp acc' params = case Map.lookup funName tenv of
            Just (EFun args _) -> foldl' matchParam acc $ zip args params
            _ -> acc'

        matchParam acc' (EAliasMeta i, Just txt) =
            IntMap.insertWith (Map.unionWith (+)) i (Map.singleton txt 1) acc'
        matchParam acc' _ = acc'
