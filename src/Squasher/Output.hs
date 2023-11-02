{-# LANGUAGE RecordWildCards   #-}
module Squasher.Output where

import Squasher.Squasher
import Squasher.Common
import qualified Erlang.Type as ET
import qualified Utils as EU
import Erlang.Pretty()
import Squasher.Types
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.HashSet                            as HashSet
import qualified Data.Map      as Map
import qualified Data.IntMap   as IntMap
import Data.Foldable(foldl')
import Data.Maybe(fromMaybe)
import Text.DocLayout

out :: SquashConfig -> Text
out SquashConfig{..} = render (Just 90) $ EU.pretty forms where
    forms = case typeAttrs ++ funAttrs of
        (a:as) -> foldl' (flip ET.Forms1) (ET.Forms0 a) as
        _ -> error "Internal error, no data"
    
    typeAttrs = map ET.Form0 $ aliasesToAttrs aliasEnv
    funAttrs = map ET.Form0 $ envToAttrs tyEnv

-- so it can be pretty printed
-- we need to also find good names and aliases here
toErl :: SquashConfig -> ([(Text, ET.TypedAttrVal)], [ET.TypeSpec])
toErl SquashConfig{..} = undefined

toType :: ErlType -> ET.Type
toType _t = case _t of
    EInt -> ET.Type7 $ ET.Atom "integer"
    EFloat -> ET.Type7 $ ET.Atom "float"
    ENamedAtom txt -> ET.Type6 $ ET.Atom $ Text.unpack txt
    EAnyAtom -> ET.Type7 $ ET.Atom "atom"
    ETuple ts -> case toTopSequence ts of
        Just tops -> ET.Type17 tops  -- $ map _ ts
        _ -> ET.Type16
    EAny -> ET.Type7 $ ET.Atom "any"
    ENone -> ET.Type7 $ ET.Atom "none"
    EUnion ts -> 
        case toTopUnion $ HashSet.toList ts of
            Just top -> ET.Type4 top
            _ -> ET.Type7 $ ET.Atom "none"
    EFun ts t -> ET.Type24 $ toFunType ts t
    -- todo naming
    EAliasMeta i -> ET.Type5 $ ET.Var $ "V" ++ show i
    EUnknown -> ET.Type5 $ ET.Var "_"
    EBinary -> ET.Type7 $ ET.Atom "binary"
    EBitString -> ET.Type7 $ ET.Atom "bitstring"
    EContainer c -> transContainer c
    EMap _ -> ET.Type15 $ ET.MapPairTypes0 $ ET.MapPairType0 (erlTypeToTop EAny) (erlTypeToTop EAny)
    EPid -> ET.Type7 $ ET.Atom "pid"
    EPort -> ET.Type7 $ ET.Atom "port"
    ERef -> ET.Type7 $ ET.Atom "reference"
    EBoolean -> ET.Type7 $ ET.Atom "boolean"

transContainer :: Container ErlType -> ET.Type
transContainer _c = case _c of
    CList t -> ET.Type12 $ erlTypeToTop t
    CDict t -> ET.Type10  (ET.Atom "dict") (ET.Atom "dict") (ET.TopTypes0 $ erlTypeToTop t)
    COldSet t -> ET.Type10  (ET.Atom "sets") (ET.Atom "set") (ET.TopTypes0 $ erlTypeToTop t)
    CGbSet t -> ET.Type10  (ET.Atom "gb_sets") (ET.Atom "set") (ET.TopTypes0 $ erlTypeToTop t)
    CGbTree t1 t2 -> ET.Type10  (ET.Atom "gb_trees") (ET.Atom "tree") (ET.TopTypes1 (erlTypeToTop t1) $ ET.TopTypes0 $ erlTypeToTop t2)
    CGb -> 
        case toTopUnion [EContainer $ CGbSet EAny, EContainer $ CGbTree EAny EAny] of
            Just top -> ET.Type4 top
            _ -> ET.Type7 $ ET.Atom "any"
    CArray t -> ET.Type10  (ET.Atom "array") (ET.Atom "array") (ET.TopTypes0 $ erlTypeToTop t)

toFunType :: [ErlType] -> ErlType -> ET.FunType
toFunType ts t = case toTopSequence ts of
    Just tops -> ET.FunType2 tops (erlTypeToTop t)
    _ -> ET.FunType1 (erlTypeToTop t)

-- these might result in unnecessary parens
toTopSequence :: [ErlType] -> Maybe ET.TopTypes
toTopSequence []     = Nothing
toTopSequence (t:ts) = Just $ foldl' (\acc t' -> ET.TopTypes1 (erlTypeToTop t') acc) (ET.TopTypes0 $ erlTypeToTop t) ts

toTopUnion :: [ErlType] -> Maybe ET.TopType
toTopUnion []     = Nothing -- any
toTopUnion (t:ts) = Just $ foldl' (\acc t' -> ET.TopType1 (toType t') acc) (erlTypeToTop t) ts


erlTypeToTop :: ErlType -> ET.TopType
--erlTypeToTop (EUnion ts) = 
erlTypeToTop t = ET.TopType2 $ toType t

-- no when guard, no named params etc
toFunction :: (FunName, ErlType) -> ET.Attribute 
toFunction (MkFunName{..}, EFun ts t) =
    ET.Attribute2 $ ET.TypeSpec 
        (ET.SpecFunc0 $ ET.Atom $ Text.unpack funName) 
        (ET.TypeSigs0 $ ET.TypeSig0 $ toFunType ts t)
toFunction _ = error "Internal error"

envToAttrs :: TyEnv -> [ET.Attribute]
envToAttrs (MkTyEnv m) = map toFunction $ Map.toList m

aliasToAttr :: (Int, ErlType) -> ET.Attribute
aliasToAttr (i, t) = ET.Attribute1 
    (ET.Atom "type") 
    (ET.TypedAttrVal1 
        (ET.Expr13 $ ET.ExprRemote1 $ ET.ExprMax1 $ ET.Atomic3 $ ET.Atom $ "V" ++ show i) 
        (erlTypeToTop t))

aliasesToAttrs :: AliasEnv -> [ET.Attribute]
aliasesToAttrs (MkAliasEnv aliasM _) = map aliasToAttr $ IntMap.toList aliasM