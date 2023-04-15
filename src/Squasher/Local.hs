{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Squasher.Local where

import Foreign.Erlang.Term
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.Trans.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Data.Data
import Data.Generics.Uniplate.Operations (transformM)
import Data.Generics.Uniplate.Data
import qualified Data.IntMap as IntMap
import qualified Data.Maybe
import Data.List (nub, intercalate, (\\), delete)
import qualified Control.Monad
import Control.Monad (when)

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
            -- Meta alias, used by the algorithm 
             | EAliasMeta Int
             | EUnknown
             deriving(Ord, Eq, Data, Typeable)

instance Show ErlType where
    show ty = case ty of
        EInt -> "integer()"
        EFloat -> "float()"
        -- escape the '-s?
        ENamedAtom a -> "'" ++ Text.unpack a ++ "'"
        EAtom -> "atom()"
        ETuple ts -> "{" ++ intercalate ", " (map show ts) ++ "}"
        EAny -> "any()"
        EUnion ts -> intercalate " | " (map show $ Set.toList ts)
        EFun ts t -> "fun((" ++ intercalate ", " (map show ts) ++ ") -> " ++ show t ++ ")"
        EAliasMeta i -> "#meta{i=" ++ show i ++ "}"
        EUnknown -> "?"

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

combines :: [ErlType] -> ErlType
combines = foldl combine EUnknown

flattenUnions :: Set ErlType -> Set ErlType
flattenUnions = Set.fold go Set.empty where
    go (EUnion ts) set = ts <> set
    go t set = Set.insert t set

makeArgs :: ErlType -> Int -> Int -> [ErlType]
makeArgs t index size =
    [ getT i | i <- [1 .. size] ] where
        getT i | i == index = t
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

newtype AliasEnv = MkAliasEnv { unAliasEnv :: IntMap ErlType }

-- TODO: nice alias for unions as well
-- EUnion [ENamedAtom "undefined", ETuple [ENamedAtom "node", ...]] -->
-- undefined_node_13()
showAlias :: Int -> ErlType -> Text
showAlias i t = case t of
    ETuple (ENamedAtom a : _) ->
        Text.pack $ "#" ++ show a ++ "_" ++ show i ++ "{}"
    _ -> Text.pack $ "t_" ++ show i ++ "()"

addAlias :: Int -> ErlType -> AliasEnv -> AliasEnv
addAlias i t (MkAliasEnv im) = MkAliasEnv $ IntMap.insert i t im

-- not good: throw error!
lookupAlias :: Int -> AliasEnv -> ErlType
lookupAlias i (MkAliasEnv im) =
    Data.Maybe.fromMaybe (error "internal error") (IntMap.lookup i im)
data SquashConfig = SquashConfig
                  { aliases :: AliasEnv
                  , functions :: TyEnv
                  , counter :: Int
                  }

-- | Change all occurences of aliases to newT in a type
substTy :: [Int] -> ErlType -> ErlType -> ErlType
substTy aliases newT = transform go where
    go (EAliasMeta alias) | alias `elem` aliases = newT
    go t = t

-- | Substitute all occurrences of aliases in every alias
substInAliases :: [Int] -> ErlType -> State SquashConfig ()
substInAliases aliases newT = do
    SquashConfig{aliases = MkAliasEnv imap} <- get
    let newAliases = IntMap.map (substTy aliases newT) imap
    modify (\conf -> conf{aliases = MkAliasEnv newAliases})
    return ()

mapAliasesTo :: [Int] -> ErlType -> State SquashConfig ()
mapAliasesTo as newT = do
    SquashConfig{aliases = MkAliasEnv imap} <- get
    let newAliases = IntMap.map
                        (\case {EAliasMeta i | i `elem` as -> newT; t -> t})
                        imap
    modify (\conf -> conf{aliases = MkAliasEnv newAliases})
    return ()

postwalk :: (ErlType -> State a ErlType) ->
            ErlType ->
            State a ErlType
postwalk = transformM

reg :: ErlType -> State SquashConfig ErlType
reg t = do
    SquashConfig{counter=ctr} <- get
    modify (\conf -> conf { counter = ctr+1
                          , aliases = addAlias ctr t (aliases conf)
                          })
    return (EAliasMeta ctr)

resolve :: ErlType -> State SquashConfig ErlType
resolve (EAliasMeta i) = do
    t <- gets (lookupAlias i . aliases)
    resolve t
resolve t = return t

resolveUnions :: ErlType -> State SquashConfig ErlType
resolveUnions t = do
    t' <- resolve t
    case t' of
        EUnion ts -> return $ EUnion ts
        _ -> return t

aliasesInTy :: ErlType -> [Int]
aliasesInTy t = nub $ para visit t where
    visit :: ErlType -> [[Int]] -> [Int]
    visit (EAliasMeta i) is = i : concat is
    visit _ is = concat is

-- Modified from the paper a bit, unions are kept flat correctly.
aliasTuples :: ErlType -> State SquashConfig ErlType
aliasTuples = postwalk f where
    f :: ErlType -> State SquashConfig ErlType
    f t@(ETuple (ENamedAtom _ : _)) = reg t
    f (EUnion ts) | any (\case { EAliasMeta _ -> True; _ -> False}) ts = do
        ts' <- traverse resolveUnions (Set.toList ts)
        reg $ EUnion $ Set.fromList ts'
    f t = return t

-- should this rather be a maybeMerge?
shouldMerge :: [ErlType] -> Bool
shouldMerge (ETuple (ENamedAtom n : elems) : ts) =
    all filt ts where
        filt :: ErlType -> Bool
        filt (ETuple (ENamedAtom n' : elems')) =
            n == n' && length elems == length elems'
        filt _ = False
shouldMerge (ENamedAtom n : ts) =
    all filt ts where
        filt :: ErlType -> Bool
        filt (ENamedAtom n') = n == n'
        filt _ = False
shouldMerge _ = False

mergeAliases :: [Int] -> State SquashConfig ()
mergeAliases [] = return ()
mergeAliases ai@(a1:as) = do
    ts <- traverse
        (\i -> do
            t <- erase <$> resolve (EAliasMeta i)
            return $ substTy [a1] (EAliasMeta i) t
        ) ai
    let sigma = combines ts
    mapAliasesTo as (EAliasMeta a1)
    -- not sure about this one
    substInAliases as (EAliasMeta a1)
    mapAliasesTo [a1] sigma
    return () where
        erase :: ErlType -> ErlType
        erase (EAliasMeta a') | a' `elem` as = EUnion Set.empty
        erase (EUnion ts) = EUnion (Set.map erase ts)
        erase t = t

-- IntSets?
-- bad recursion scheme
squash :: [Int] -> [Int] -> State SquashConfig ()
squash [] _done = return ()
squash (a1 : worklist) done = do
    SquashConfig{..} <- get
    let as = aliasesInTy (lookupAlias a1 aliases) \\ done
    let ap = Data.List.delete a1 done
    when (a1 `notElem` done) (mapM_ f (ap ++ as))
    squash (worklist ++ as) (done ++ [a1])
    where
        f a2 = do
            ts <- mapM (resolve . EAliasMeta) [a1, a2]
            when (shouldMerge ts) $ mergeAliases [a1, a2]

squashAll :: ErlType -> State SquashConfig ()
squashAll t =  mapM_ (\a -> squash [a] []) (aliasesInTy t)

squashLocal :: TyEnv -> State SquashConfig ()
squashLocal (MkTyEnv e) =
    mapM_ h (Map.toList e) where
        h :: (FunName, ErlType) -> State SquashConfig ()
        h (x, t) = do
            t' <- aliasTuples t
            squashAll t'
            SquashConfig{..} <- get
            modify (\conf -> conf{functions=MkTyEnv $ Map.insert x t' (unTyEnv functions)})