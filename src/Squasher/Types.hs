{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Squasher.Types where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Data
import Data.Map(Map)
import Data.List (intercalate)
import Foreign.Erlang.Term


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
             | EList (Set ErlType)
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
        (Tuple [Atom _ "atom", Atom _ value]) -> Just $ ENamedAtom value
        (Tuple [Atom _ "union", List terms Nil]) -> EUnion . Set.fromList <$> mapM fromTerm terms
        (Tuple [Atom _ "func", List args Nil, res]) -> EFun <$> mapM fromTerm args <*> fromTerm res
        (Tuple [Atom _ "tuple", List terms Nil]) -> ETuple <$> mapM fromTerm terms
        (Atom _ "unknown") -> Just EUnknown
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
        EMap _ -> "map()"
        EList _ -> "list()" 