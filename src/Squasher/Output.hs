{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Squasher.Output(out) where

import qualified Data.HashSet        as HashSet
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap
import qualified Data.Map            as Map
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Foreign.Erlang.Term (AtomType (..), Term (..))
import qualified Foreign.Erlang.Term as Erlang
import           Squasher.Common
import           Squasher.Naming
import           Squasher.Types

out :: IntMap Name -> SquashConfig -> Term
out names SquashConfig{..} =
    Erlang.List (aliasesToForms names aliasEnv ++ envToForms names tyEnv) Erlang.Nil

envToForms :: IntMap Name -> TyEnv -> [Term]
envToForms names (MkTyEnv m) = map doSpec $ Map.toList m where
    doSpec :: (FunName, ErlType) -> Term
    doSpec (MkFunName{..}, EFun ts t) =
        taggedTuple names "attribute"
            [ atom "spec"
            , Erlang.Tuple
                [ Erlang.Tuple [atom funName, Erlang.Integer $ toInteger funArity]
                , Erlang.List [funType names ts t] Erlang.Nil
                ]
            ]
    doSpec _ = error "Internal error, not a function"

aliasesToForms :: IntMap Name -> AliasEnv -> [Term]
aliasesToForms names MkAliasEnv{..} = map typeAlias $ IntMap.toList aliasMap where
    typeAlias :: (Int, ErlType) -> Term
    typeAlias (i, t) = case names IntMap.! i of
        Alias txt ->
            taggedTuple names "attribute"
                [atom "type", Erlang.Tuple [atom txt, toTerm names t, Erlang.Nil]]
        Record txt ->
            taggedTuple names "attribute"
                [ atom "record"
                , Erlang.Tuple
                    [ atom txt
                    , Erlang.List (map (recordFieldToTerm names) $ recordFields t) Erlang.Nil
                    ]
                ]

recordFields :: ErlType -> [(Int, ErlType)]
recordFields (ETuple ts) = zip [1, 2..] ts
recordFields _           = error "Internal error, not a tuple"

recordFieldToTerm :: IntMap Name -> (Int, ErlType) -> Term
recordFieldToTerm n (i, t) =
    taggedTuple n "type"
        [ atom "field_type"
        , Erlang.List [atom $ Text.pack $ show i, toTerm n t] Erlang.Nil
        ]

class ToTerm a where
    toTerm :: IntMap Name -> a -> Term

atom :: Text -> Term
atom = Erlang.Atom Erlang.AtomUtf8

-- separate one for builtins?
userType :: IntMap Name -> Text -> [ErlType] -> Term
userType n txt ts =
    Erlang.Tuple [ atom "user_type"
                 , Erlang.Nil
                 , atom txt
                 , Erlang.List (map (toTerm n) ts) Erlang.Nil
                 ]

remoteType :: IntMap Name -> Text -> Text -> [ErlType] -> Term
remoteType n m f args =
    Erlang.Tuple [ atom "remote_type"
                 , Erlang.Nil
                 , Erlang.List
                    [atom m, atom f, Erlang.List (map (toTerm n) args) Erlang.Nil]
                    Erlang.Nil
                 ]

builtin :: IntMap Name -> Text -> [ErlType] -> Term
builtin n txt ts =
    Erlang.Tuple [ atom "type"
                 , Erlang.Nil
                 , atom txt
                 , Erlang.List (map (toTerm n) ts) Erlang.Nil
                 ]

taggedTuple :: ToTerm a => IntMap Name -> Text -> [a] -> Term
taggedTuple n txt as =
    Erlang.Tuple $ [atom txt, Erlang.Nil] ++ map (toTerm n) as

instance ToTerm ErlType where
    toTerm n _t = case _t of
        EInt -> builtin n "integer" []
        EFloat -> builtin n "float" []
        -- escape the '-s?
        ENamedAtom a -> atom a
        EAnyAtom -> builtin n "atom" []
        ETuple ts -> taggedTuple n "type" [atom "tuple", Erlang.List (map (toTerm n) ts) Erlang.Nil]
        EAny -> builtin n "any" []
        ENone -> builtin n "none" []
        EUnion ts ->
            taggedTuple n "type"
                [ atom "union"
                , Erlang.List (map (toTerm n) $ HashSet.toList ts) Erlang.Nil
                ]
        EFun ts t -> funType n ts t
        EAliasMeta i ->
            case n IntMap.! i of
                Alias txt -> userType n txt []
                Record txt -> taggedTuple n "type" [atom "record", Erlang.List [atom txt] Erlang.Nil]
        EUnknown -> userType n "unknown" []
        EBinary -> userType n "binary" []
        EBitString -> userType n "bitstring" []
        EPid -> builtin n "pid" []
        EPort -> builtin n "port" []
        ERef -> builtin n "reference" []
        EBoolean -> userType n "boolean" []
        EMap _ -> taggedTuple n "type" [atom "map", atom "any"]
        EContainer c -> toTerm n c

funType :: IntMap Name -> [ErlType] -> ErlType -> Term
funType n ts t =
    taggedTuple n "type"
        [ atom "'fun'" -- or 'fun'
        , Erlang.List [ taggedTuple n "type" [ atom "product", Erlang.List (map (toTerm n) ts) Erlang.Nil ]
                      , toTerm n t
                      ]
          Erlang.Nil
        ]

instance ToTerm Term where
    toTerm _ t = t

instance ToTerm (Container ErlType) where
    toTerm n _c = case _c of
        CList t -> builtin n "list" [t]
        CDict t -> remoteType n "dict" "dict" [t]
        COldSet t -> remoteType n "sets" "set" [t]
        CGbSet t -> remoteType n "gb_sets" "set" [t]
        CGbTree t1 t2 -> remoteType n "gb_trees" "tree" [t1, t2]
        CGb ->
            taggedTuple n "type"
                [ atom "union"
                , Erlang.List (map (toTerm n) [CGbSet EAny, CGbTree EAny EAny]) Erlang.Nil
                ]
        CArray t -> remoteType n "array" "array" [t]
