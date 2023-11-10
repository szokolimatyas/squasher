{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Squasher.Naming(Name(..), nameAll) where

import qualified Data.HashSet    as HashSet
import           Data.IntMap     (IntMap)
import qualified Data.IntMap     as IntMap
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Squasher.Common
import           Squasher.Types

data Name = Record Text
          | Alias Text
          deriving (Eq, Ord, Show)

nameAll :: SquashConfig -> IntMap Name
nameAll SquashConfig{aliasEnv=MkAliasEnv{..}, options=o} = IntMap.foldlWithKey (nameAlias o) initialNames aliasMap

initialNames :: IntMap Name
initialNames = IntMap.fromList $ zip [-1, -2..] $ map Alias
    [ "any", "none", "dynamic", "pid", "port", "reference", "atom",  "float", "fun", "integer", "list", "map", "tuple"]

nameAlias :: Options -> IntMap Name -> Int -> ErlType -> IntMap Name
nameAlias Options{..} names i t = case t of
    (ETuple (ENamedAtom txt : ts)) | length ts >= recordSize ->
        insertNew i (Record txt) names
    (ETuple (ENamedAtom txt : _)) ->
        insertNew i (Alias txt) names
    (ETuple (EUnion uni : _)) | Just ns <- mapM getTag $ HashSet.toList uni ->
        insertNew i (Alias $ fitName t $ fitNames ns <> "_tuple") names
    (EUnion uni) | Just ns <- mapM getTag $ HashSet.toList uni ->
        insertNew i (Alias $ fitName t $ fitNames ns) names
    (EUnion uni) ->
        let ns = map simpleName $ HashSet.toList uni in
        insertNew i (Alias $ fitName t $ fitNames ns) names
    _ ->
        insertNew i (Alias $ simpleName t) names

insertNew :: Int -> Name -> IntMap Name -> IntMap Name
insertNew i (Record txt) m =
    if Record txt `elem` m then
        -- I know there is a better way
        IntMap.insert i (Record $ txt <> Text.pack (show i)) m
    else
        IntMap.insert i (Record txt) m
insertNew i (Alias txt) m =
    if Alias txt `elem` m then
        IntMap.insert i (Alias $ txt <> Text.pack (show i)) m
    else
        IntMap.insert i (Alias txt) m

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
