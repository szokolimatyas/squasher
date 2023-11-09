{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Squasher.Types(ErlType(..), Container(..), mkUnion) where

import           Data.Data
import           Data.Hashable
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet
import           Data.List           (intercalate)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified Data.Maybe          as Maybe
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Foreign.Erlang.Term
import           GHC.Generics        (Generic)

data ErlType = EInt
             | EFloat
             | ENamedAtom Text
             | EAnyAtom
             | ETuple [ErlType]
            -- | EList ErlType
             | EAny
             | ENone
             | EUnion (HashSet ErlType)
             | EFun [ErlType] ErlType
            -- Meta alias, used by the algorithm
             | EAliasMeta Int
             | EUnknown
            --
             | EBinary
             | EBitString
            -- is_builtin can be used when wrapping functions
             -- support for improper lists?
             -- array, or
             | EContainer (Container ErlType)
             -- keyword maps?
             | EMap (Map ErlType ErlType)
             | EPid
             | EPort
             | ERef
             | EBoolean
             deriving(Ord, Eq, Data, Typeable, Generic)

-- improper lists? maybe_improper_lists?
data Container a = CList a
                 | CDict a
                 | COldSet a
                 | CGbSet a
                 | CGbTree a a
                 -- for empty gb_set and gb_tree, we cannot differentiate between the representations
                 | CGb
                 | CArray a

instance Hashable ErlType

deriving instance Ord a => Ord (Container a)
deriving instance Eq a => Eq (Container a)
deriving instance Data a => Data (Container a)
deriving instance Typeable a => Typeable (Container a)
deriving instance Generic a => Generic (Container a)
instance (Hashable a,  Generic a, Typeable a) => Hashable (Container a)

instance FromTerm ErlType where
    fromTerm t = case t of
        (Atom _ "integer") -> Just EInt
        (Atom _ "float") -> Just EFloat
        (Tuple [Atom _ "atom", Atom _ value]) -> Just $ ENamedAtom value
        (Tuple [Atom _ "tuple", Nil]) -> Just $ ETuple []
        (Tuple [Atom _ "tuple", List terms Nil]) -> ETuple <$> mapM fromTerm terms
        (Atom _ "any") -> Just EAny
        (Atom _ "none") -> Just ENone
        (Tuple [Atom _ "union", Nil]) -> Just $ EUnion HashSet.empty
        (Tuple [Atom _ "union", List terms Nil]) -> EUnion . HashSet.fromList <$> mapM fromTerm terms
        (Tuple [Atom _ "function", Nil, res]) -> EFun [] <$> fromTerm res
        (Tuple [Atom _ "function", List args Nil, res]) -> EFun <$> mapM fromTerm args <*> fromTerm res
        (Atom _ "unknown") -> Just EUnknown
        (Atom _ "binary") -> Just EBinary
        (Atom _ "bitstring") -> Just EBitString
        (Tuple [Atom _ "list",  term]) ->
            EContainer . CList <$> fromTerm term
        (Tuple [Atom _ "map", Nil]) ->
            Just $ EMap Map.empty
        (Tuple [Atom _ "map", List terms Nil]) ->
            Just $ EMap $ Map.fromList $ Maybe.mapMaybe
                (\case { Tuple [t1,t2] -> (,) <$> fromTerm t1 <*> fromTerm t2; _ -> Nothing}) terms
        (Tuple [Atom _ "dict",  term]) ->
            EContainer . CDict <$> fromTerm term
        (Tuple [Atom _ "set",  term]) ->
            EContainer . COldSet <$> fromTerm term
        (Tuple [Atom _ "gb_set",  term]) ->
            EContainer . CGbSet <$> fromTerm term
        (Tuple [Atom _ "gb_tree",  term1, term2]) -> do
            t1 <- fromTerm term1
            t2 <- fromTerm term2
            return $ EContainer $ CGbTree t1 t2
        (Atom _ "gb_empty") ->
            Just $ EContainer CGb
        (Tuple [Atom _ "array",  term]) ->
            EContainer . CArray <$> fromTerm term
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
        ENone -> "none()"
        EUnion ts -> intercalate " | " (map show $ HashSet.toList ts)
        EFun [t1] t2 -> "fun(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
        EFun ts t -> "fun((" ++ intercalate ", " (map show ts) ++ ") -> " ++ show t ++ ")"
        EAliasMeta i -> "$" ++ show i
        EUnknown -> "_"
        EBinary -> "binary()"
        EBitString -> "bitstring()"
        EPid -> "pid()"
        EPort -> "port()"
        ERef -> "reference()"
        EBoolean -> "boolean()"
        EMap ts -> "#{" ++ intercalate ", " (map (\(t1, t2) -> show t1 ++ " => " ++ show t2) (Map.toList ts)) ++ "}"
        EContainer c -> show c

instance Show a => Show (Container a) where
    show (CList t)       = "list(" ++ show t ++ ")"
    show (CDict t)       = "dict:dict(" ++ show t ++ ")"
    show (COldSet t)     = "sets:set(" ++ show t ++ ")"
    show (CGbSet t)      = "gb_sets:set(" ++ show t ++ ")"
    show (CGbTree t1 t2) = "gb_trees:tree(" ++ show t1 ++ ", " ++ show t2 ++ ")"
    show CGb             = "gb_sets:set(?) | gb_trees:tree(?)"
    show (CArray t)      = "array:array(" ++ show t ++ ")"

mkUnion :: HashSet ErlType -> ErlType
mkUnion ts | HashSet.size ts == 1 = head $ HashSet.toList ts
mkUnion ts = EUnion ts
