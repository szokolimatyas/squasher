{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Squasher.Local where

import Squasher.Types
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.Set (Set)
import Control.Monad.Trans.State.Strict
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.ByteString.Lazy (ByteString)
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Generics.Uniplate.Operations (transformM)
import Data.Generics.Uniplate.Data
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Maybe
import Data.List (nub, intercalate, (\\), delete)
import Control.Monad (when)
import Data.Binary (decodeOrFail)
import Data.Binary.Get (ByteOffset)
import Control.Monad.Trans.Except (Except, throwE, except) 
import Data.Foldable (traverse_)
import Foreign.Erlang.Term
import Debug.Trace

myTrace _ = return ()
--myTrace = Debug.traceM

newtype Path = MkPath { pathParts :: [PathPart] }
    deriving(Eq, Show)

data FunName = MkFunName { funName :: Text
                         , funArity :: Int
                         } deriving(Eq, Ord)

instance Show FunName where
    show MkFunName{funName, funArity} =
        Text.unpack funName ++ "/" ++ show funArity

data PathPart =
                Dom Int Int -- ^ Parameter %1 of a function with arity %2
              | Range Int   -- ^ Result in a function with arity %1
              | FunN FunName -- ^ Function name and arity
              | Rec ErlType Int Int -- ^ Record with key, at position, field number
              | ListElement -- ^ List
              | MapElement ErlType -- ^ Not a shapemap yet!
              deriving(Eq, Show)

instance FromTerm PathPart where
    fromTerm t = case t of
        Tuple [Atom _ "rng", Integer i] -> Just $ Range $ fromInteger i
        Tuple [Atom _ "dom", Integer i, Integer j] -> Just $ Dom (fromInteger i) (fromInteger j)
        Tuple [Atom _ "name", String s, Integer i] ->
            Just $ FunN (MkFunName s (fromInteger i))
        Tuple [Atom _ "tuple_index", keyTerm, Integer i, Integer j] ->
            fromTerm keyTerm >>= \et -> Just $ Rec et (fromInteger i) (fromInteger j)
       -- Tuple [Atom _ "tuple_index", Tuple [Atom _ "atom", Atom _ key], Integer i, Integer j] ->
       --     Just $ Rec (Just key) (fromInteger i) (fromInteger j)
        Atom _ "list_element" -> Just ListElement
        Tuple [Atom _ "map_element", key] -> 
            MapElement <$> fromTerm key
        -- TODO: what about when the tuple index is not an atom?
        -- Tuple [Atom _ "rec", Atom _ "undefined", Integer i, Integer j] ->
        --     Just $ Rec Nothing (fromInteger i) (fromInteger j)
        -- Tuple [Atom _ "rec", String key, Integer i, Integer j] ->
        --     Just $ Rec (Just key) (fromInteger i) (fromInteger j)
        _ -> Nothing

instance FromTerm Path where
    fromTerm t = case t of
        List elems _ ->
            let
                parts =  mapM fromTerm elems
            in
                MkPath <$> parts
        _ -> Nothing


entryFromTerm :: Term -> Except String (ErlType, Path)
entryFromTerm (Tuple [t1, t2]) = do
   erlTy <- except $ maybeToEither ("Not an erlang type: " ++ show t1) (fromTerm t1)
   path <- except $ maybeToEither ("Not a path: " ++ show t2) (fromTerm t2)
   return (erlTy, path)
entryFromTerm t = throwE $ "Not an entry: " ++ show t

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _def (Just val) = Right val
maybeToEither def Nothing = Left def

runner :: ByteString -> Except String SquashConfig
runner bs = case res of
    Right (_, _, MkExternalTerm (List terms Nil)) -> do
        entries <- mapM entryFromTerm terms
        myTrace $ "Entries:\n" ++ show entries ++ "\n"
        let env = foldl (\tenv (t, p) -> update t p tenv) (MkTyEnv Map.empty) entries
        let env' = env -- MkTyEnv (Map.take 1 $ Map.drop 2 (unTyEnv env))
        traceM $ "Env:\n" ++ show env' ++ "\n"
        return $ execState squashLocal (SquashConfig (MkAliasEnv IntMap.empty) env' 0)
    Right (_, _, MkExternalTerm terms) -> throwE $ "Terms are in a wrong format: " ++ show terms
    Left (_, _, str) -> throwE $ "Could not parse bytestring, error: " ++ str
  where
    res ::  Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, ExternalTerm)
    res = decodeOrFail bs


newtype TyEnv = MkTyEnv { unTyEnv :: Map FunName ErlType }
instance Show TyEnv where
    show (MkTyEnv m) =
        intercalate "\n" (map (\(i, t) -> show i ++ " -> " ++ show t) $ Map.toList m) ++
        "\n"

-- todo: binary/bitrstring
-- float/integer --> num?
combine :: ErlType -> ErlType -> ErlType
combine EUnknown t = t
combine t EUnknown = t
combine t1 t2 | t1 == t2 = t1
combine EBitString EBinary = EBitString
combine EBinary EBitString = EBitString
combine (ENamedAtom a1) (ENamedAtom a2) 
    | a1 `elem` ["true", "false"] && 
      a2 `elem` ["true", "false"] = EBoolean
-- not sure
combine (ETuple (EUnknown : ts1)) (ETuple (t2 : ts2)) | length ts1 == length ts2 =
    ETuple $ t2 : zipWith combine ts1 ts2
combine (ETuple (t1 : ts1)) (ETuple (EUnknown : ts2)) | length ts1 == length ts2 =
    ETuple $ t1 : zipWith combine ts1 ts2
combine (ETuple (ENamedAtom a1 : ts1)) (ETuple (ENamedAtom a2 : ts2)) | a1 == a2 && length ts1 == length ts2 =
    ETuple $ ENamedAtom a1 : zipWith combine ts1 ts2
combine (EList t1) (EList t2) = EList $ t1 `combine` t2
-- no support for shapemaps!
-- this is not the best, there could be too many different key types
combine (EMap m1) (EMap m2) =
    -- TODO: magic number
    -- collapse large maps
    if Map.size m1 + Map.size m2 < 5 
    then EMap $ Map.unionWith combine m1 m2
    else EMap $ uncurry Map.singleton (combineMap $ Map.union m1 m2)
combine (EUnion ts) t1 = EUnion $ flattenUnions $ Set.map (`combine` t1) ts
combine t1 (EUnion ts) = EUnion $ flattenUnions $ Set.map (`combine` t1) ts
combine (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
    EFun (zipWith combine argts1 argts2) (combine t1 t2)
combine t1 t2 = EUnion $ flattenUnions $ Set.fromList [t1, t2]

combineMap :: Map ErlType ErlType -> (ErlType, ErlType)
combineMap = Map.foldrWithKey (\k' v' (k, v) -> (k `combine` k', v `combine` v')) (EUnknown, EUnknown)

combines :: [ErlType] -> ErlType
combines [] = EUnknown
combines (t:ts) = foldl combine t ts

flattenUnions :: Set ErlType -> Set ErlType
flattenUnions s = if Set.size newUnion > 20 then Set.singleton EAny else newUnion where 
    newUnion = Set.fold go Set.empty s
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
            update (ETuple $ k : makeArgs ty (index-1) (size-1))
                   (MkPath p') env
        Dom pos arity : p' ->
            update (EFun (makeArgs ty pos arity) EUnknown) (MkPath p') env
        Range arity : p' ->
            update (EFun (replicate arity EUnknown) ty) (MkPath p') env
        ListElement : p' ->
            update (EList ty) (MkPath p') env
        MapElement k : p' ->
            update (EMap $ Map.singleton k ty) (MkPath p') env
        [FunN fn] -> let tenv = unTyEnv env in
            case Map.lookup fn tenv of
                Just ty' -> MkTyEnv $ Map.insert fn (combine ty ty') tenv
                Nothing -> MkTyEnv $ Map.insert fn ty tenv
        _ -> error "Internal error"


newtype AliasEnv = MkAliasEnv { unAliasEnv :: IntMap ErlType }

instance Show AliasEnv where
    show (MkAliasEnv imap) =
        intercalate "\n" (map (\(i, t) -> "$" ++ show i ++ " -> " ++ show t) $ IntMap.toList imap) ++
        "\n"

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
                  } deriving(Show)

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
    let newAliases = IntMap.mapWithKey
                        (\i t -> if i `elem` as then newT else t)
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

aliasesInTy :: ErlType -> IntSet
aliasesInTy = para visit where
    visit :: ErlType -> [IntSet] -> IntSet
    visit (EAliasMeta i) is = IntSet.insert i (IntSet.unions is)
    visit _ is = IntSet.unions is

-- | All aliases in a type, and the aliases in the aliases, etc...
aliasesInTyRec :: IntSet -> ErlType -> SquashConfig -> IntSet
aliasesInTyRec is t s = case t of
    ETuple ts -> IntSet.unions $ map (\t' -> aliasesInTyRec is t' s) ts
    EUnion ts -> IntSet.unions $ map (\t' -> aliasesInTyRec is t' s) (Set.toList ts)
    EFun argts rest ->  IntSet.unions $ aliasesInTyRec is rest s : map (\t' -> aliasesInTyRec is t' s) argts
    EList t' -> aliasesInTyRec is t' s
    EMap m -> 
        IntSet.unions $ map (\(t1, t2) -> IntSet.union (aliasesInTyRec is t1 s) (aliasesInTyRec is t2 s)) $ Map.toList m
    EAliasMeta i -> 
        if IntSet.member i is
        then is
        else 
            let t' = lookupAlias i (aliases s) 
                is' = aliasesInTyRec (IntSet.insert i is) t' s
            in is'
    _ -> is

-- Modified from the paper a bit, unions are kept flat correctly.
-- But maybe if we do the old way, there is a pont to realiasing in the global step?
aliasTuples :: ErlType -> State SquashConfig ErlType
aliasTuples = postwalk f where
    f :: ErlType -> State SquashConfig ErlType
    f t@(ETuple (ENamedAtom _ : _)) = reg t
    f (EUnion ts) =
        if any (\case { EAliasMeta _ -> True; _ -> False}) ts then do
            ts' <- traverse resolveUnions (Set.toList ts)
            reg $ EUnion $ flattenUnions $ Set.fromList ts'
        else
            return $ EUnion $ flattenUnions ts
    f t = return t

pruneAliases :: State SquashConfig ()
pruneAliases = do
    s <- get
    let tys = Map.elems $ unTyEnv $ functions s
    let usedAliases = IntSet.unions $ map (\t -> aliasesInTyRec IntSet.empty t s) tys
    put s{aliases = MkAliasEnv (IntMap.restrictKeys (unAliasEnv $ aliases s) usedAliases)}

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
        erase (EUnion ts) = EUnion $ flattenUnions $ Set.map erase ts
        erase t = t

-- IntSets?
-- bad recursion scheme
squash :: [Int] -> IntSet -> State SquashConfig ()
squash [] _done = return ()
squash (a1 : worklist) done = do
    SquashConfig{..} <- get
    let as = aliasesInTy (lookupAlias a1 aliases) IntSet.\\ done
    let ap = IntSet.delete a1 done
    when (IntSet.notMember a1 done) (reduceM_ f (IntSet.union ap as))
    squash (worklist ++ IntSet.toList as) (IntSet.insert a1 done)
    where
        f a2 = do
            ts <- mapM (resolve . EAliasMeta) [a1, a2]
            myTrace $ "squash? " ++ show a1 ++ ", " ++ show a2 ++ "\n" ++
                     "values:\n" ++ show ts ++ "\n"
            when (shouldMerge ts) (do
                myTrace $ "merging "  ++ show a1 ++ ", " ++ show a2 ++ "\n" ++
                         "values:\n" ++ show ts ++ "\n"
                mergeAliases [a1, a2])

reduceM_ :: (Int -> State SquashConfig ()) -> IntSet -> State SquashConfig ()
reduceM_ f is = do
    s <- get
    let res = IntSet.foldl (\conf key -> execState (f key) conf) s is
    put res

squashAll :: ErlType -> State SquashConfig ()
squashAll t =  reduceM_ (\a -> squash [a] IntSet.empty) (aliasesInTy t)

squashLocal :: State SquashConfig ()
squashLocal = do
    (MkTyEnv e) <- gets functions
    mapM_ h (Map.toList e)
    pruneAliases 
    s <- get
    myTrace $ "Result:\n" ++ show s ++ "\n"
    where
        h :: (FunName, ErlType) -> State SquashConfig ()
        h (x, t) = do
            t' <- aliasTuples t
            myTrace $ "aliased type:\n" ++ show t' ++ "\n"
            oldAliases <- gets aliases
            myTrace $ "show aliases:\n" ++ show oldAliases ++ "\n"
            squashAll t'
            SquashConfig{..} <- get
            modify (\conf -> conf{functions=MkTyEnv $ Map.insert x t' (unTyEnv functions)})

-------------------------------------------------------------------------------
-- Global squashing
-------------------------------------------------------------------------------

-- In the base algo, the state change caused by postwalk is ignored
-- this might be a problem
singleRec :: ErlType -> State SquashConfig ErlType
singleRec = postwalk f where
    f t'@(ETuple (ENamedAtom _ : _)) = reg t'
    f t' = return t'

singleRec' :: FunName -> ErlType -> State SquashConfig ()
singleRec' fn t = do
    sigma <- postwalk f t
    modify (\s -> s{functions=MkTyEnv $ Map.insert fn sigma (unTyEnv $ functions s)})
    return () where
        f t'@(ETuple (ENamedAtom _ : _)) = reg t'
        f t' = return t'

aliasSingleRec :: State SquashConfig ()
aliasSingleRec = do
    s@SquashConfig{..} <- get
    traverse_ (uncurry singleRec') (Map.toList $ unTyEnv functions)
    aliases' <- traverse f (unAliasEnv aliases) 
    put s{aliases=MkAliasEnv aliases'} 
    where
        f (EUnion ts) = do
            ts' <- traverse singleRec $ Set.toList ts
            return $ EUnion $ flattenUnions $ Set.fromList ts'
        f t = singleRec t
