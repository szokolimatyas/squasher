{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Squasher.Types where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Data
import Data.Map(Map)
import Data.List (intercalate)
import qualified Data.Maybe as Maybe
import Foreign.Erlang.Term
import qualified Data.Map as Map


data ErlType = EInt
             | EFloat
             | ENamedAtom Text
             | EAnyAtom
             | ETuple [ErlType]
            -- | EList ErlType
             | EAny
             | EUnion (Set ErlType)
             | EFun [ErlType] ErlType
            -- Meta alias, used by the algorithm 
             | EAliasMeta Int
             | EUnknown
            --
             | EBinary
             | EBitString
            -- is_builtin can be used when wrapping functions
             -- support for improper lists?
             | EList ErlType
             -- keyword maps?
             | EMap (Map ErlType ErlType)
             | EPid
             | EPort
             | ERef
             | EBoolean
             deriving(Ord, Eq, Data, Typeable)

instance FromTerm ErlType where
    fromTerm t = case t of
        (Atom _ "integer") -> Just EInt
        (Atom _ "float") -> Just EFloat
        (Tuple [Atom _ "atom", Atom _ value]) -> Just $ ENamedAtom value
        (Tuple [Atom _ "tuple", Nil]) -> Just $ ETuple []
        (Tuple [Atom _ "tuple", List terms Nil]) -> ETuple <$> mapM fromTerm terms
        (Atom _ "any") -> Just EAny
        (Tuple [Atom _ "union", Nil]) -> Just $ EUnion Set.empty
        (Tuple [Atom _ "union", List terms Nil]) -> EUnion . Set.fromList <$> mapM fromTerm terms
        (Tuple [Atom _ "function", Nil, res]) -> EFun [] <$> fromTerm res
        (Tuple [Atom _ "function", List args Nil, res]) -> EFun <$> mapM fromTerm args <*> fromTerm res
        (Atom _ "unknown") -> Just EUnknown
        (Atom _ "binary") -> Just EBinary
        (Atom _ "bitstring") -> Just EBitString
        (Tuple [Atom _ "list",  term]) -> 
            EList <$> fromTerm term
        (Tuple [Atom _ "map", Nil]) -> 
            Just $ EMap Map.empty
        (Tuple [Atom _ "map", List terms Nil]) -> 
            Just $ EMap $ Map.fromList $ Maybe.mapMaybe 
                (\case { Tuple [t1,t2] -> (,) <$> fromTerm t1 <*> fromTerm t2; _ -> Nothing}) terms
        (Atom _ "pid") -> Just EPid
        (Atom _ "port") -> Just EPort
        (Atom _ "reference") -> Just ERef
        (Atom _ "boolean") -> Just EBoolean
        _ -> Nothing


instance Show ErlType where
    show ty = case ty of
        EInt -> "integer()"
        EFloat -> "float()"
        -- escape the '-s?
        ENamedAtom a -> "'" ++ Text.unpack a ++ "'"
        EAnyAtom -> "atom()"
        ETuple ts -> "{" ++ intercalate ", " (map show ts) ++ "}"
        EAny -> "any()"
        EUnion ts -> intercalate " | " (map show $ Set.toList ts)
        EFun [t1] t2 -> "fun(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
        EFun ts t -> "fun((" ++ intercalate ", " (map show ts) ++ ") -> " ++ show t ++ ")"
        EAliasMeta i -> "$" ++ show i
        EUnknown -> "?"
        EBinary -> "binary()"
        EBitString -> "bitstring()"
        EPid -> "pid()"
        EPort -> "port()"
        ERef -> "reference()"
        EBoolean -> "boolean()"
        EMap ts -> "#{" ++ intercalate ", " (map (\(t1, t2) -> show t1 ++ " => " ++ show t2) (Map.toList ts)) ++ "}"
        EList t -> "list(" ++ show t ++ ")" 