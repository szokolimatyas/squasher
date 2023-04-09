{-# LANGUAGE OverloadedStrings #-}

module Squasher.Local where

import Foreign.Erlang.Term
import Data.Text (Text)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.ByteString (ByteString)

newtype Path = MkPath { pathParts :: [PathPart] }
    deriving(Eq, Show)

data FunName = MkFunName { funName :: Text
                         , funArity :: Int
                         } deriving(Eq, Ord, Show)

data PathPart =
              -- ^ Parameter %1 of a function with arity %2
                Dom Int Int
              -- ^ Result in a function with arity %1
              | Range Int
              -- ^ Function name and arity
              | FunN FunName
              -- ^ Record with key %1, at position %2, field number %3
              | Rec (Maybe Text) Int Int
              deriving(Eq, Show)

class FromTerm a where
    fromTerm :: Term -> Maybe a

instance FromTerm PathPart where
    fromTerm t = case t of
        Tuple [Atom _ "rng", Integer i] -> Just $ Range $ fromInteger i
        Tuple [Atom _ "dom", Integer i, Integer j] -> Just $ Dom (fromInteger i) (fromInteger j)
        Tuple [Atom _ "fun", String s, Integer i] -> Just $ FunN (MkFunName s (fromInteger i))
        Tuple [Atom _ "rec", Atom _ "undefined", Integer i, Integer j] ->
            Just $ Rec Nothing (fromInteger i) (fromInteger j)
        Tuple [Atom _ "rec", String key, Integer i, Integer j] ->
            Just $ Rec (Just key) (fromInteger i) (fromInteger j)
        _ -> Nothing

instance FromTerm Path where
    fromTerm t = case t of
        List elems _ ->
            let
                parts =  mapM fromTerm elems
            in
                MkPath <$> parts
        _ -> Nothing

data ErlType = EInt
             | EFloat
             | ENamedAtom Text
             | EAtom
             | ETuple [ErlType]
            -- | EList ErlType
             | EAny
             | EUnion (Set ErlType)
             | EFun [ErlType] ErlType
             | EUnknown
             deriving(Ord, Eq, Show)

newtype TyEnv = MkTyEnv { unTyEnv :: Map FunName ErlType }

combine :: ErlType -> ErlType -> ErlType
combine EUnknown t = t
combine t EUnknown = t
combine t1 t2 | t1 == t2 = t1
combine (ETuple (ENamedAtom a1 : ts1)) (ETuple (ENamedAtom a2 : ts2)) | a1 == a2 && length ts1 == length ts2 =
    ETuple $ ENamedAtom a1 : zipWith combine ts1 ts2
combine (EUnion ts) t1 = EUnion $ flattenUnions $ Set.map (`combine` t1) ts
combine t1 (EUnion ts) = EUnion $ flattenUnions $ Set.map (`combine` t1) ts
combine (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
    EFun (zipWith combine argts1 argts2) (combine t1 t2)
combine t1 t2 = EUnion $ flattenUnions $ Set.fromList [t1, t2]

flattenUnions :: Set ErlType -> Set ErlType
flattenUnions = Set.fold go Set.empty where
    go (EUnion ts) set = ts <> set
    go t set = Set.insert t set

makeArgs :: ErlType -> Int -> Int -> [ErlType]
makeArgs t index size =
    [ get i | i <- [1 .. size] ] where
        get i | i == index = t
              | otherwise = EUnknown

update :: ErlType -> Path -> TyEnv -> TyEnv
update ty (MkPath p) env =
    case p of
        Rec k index size : p' ->
            update (ETuple $ recNameToType k : makeArgs ty (index-1) (size-1))
                   (MkPath p') env
        Dom pos arity : p' ->
            update (EFun (makeArgs ty pos arity) EUnknown) (MkPath p') env
        Range arity : p' ->
            update (EFun (replicate arity EUnknown) ty) (MkPath p') env
        [FunN fn] ->
            MkTyEnv $ Map.alter visit fn (unTyEnv env) where
                visit Nothing = Nothing
                visit (Just ty') = Just $ combine ty ty'
        _ -> error "Internal error"

recNameToType :: Maybe Text -> ErlType
recNameToType Nothing = EUnknown
recNameToType (Just txt) = ENamedAtom txt

toEnv :: ByteString -> TyEnv
toEnv bs = undefined