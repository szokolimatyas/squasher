{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Squasher.Common where

import           Control.Monad               (foldM, zipWithM)
import           Control.Monad.Trans.Except  (Except, except, throwE)
import           Data.Generics.Uniplate.Data
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HashSet
import           Data.IntMap.Strict          (IntMap)
import qualified Data.IntMap.Strict          as IntMap
import           Data.IntSet                 (IntSet)
import qualified Data.IntSet                 as IntSet
import           Data.List                   (intercalate)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Foreign.Erlang.Term
import           Squasher.Types


newtype Path = MkPath { pathParts :: [PathPart] }
    deriving(Eq, Show)

data FunName = MkFunName { funName  :: Text
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
              | DictElement
              | SetElement
              | GbSetElement
              | GbTreeElement Bool -- True if first
              | ArrayElement
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
        Atom _ "dict_element" -> Just DictElement
        Atom _ "set_element" -> Just SetElement
        Atom _ "gb_set_element" -> Just GbSetElement
        Tuple [Atom _ "gb_tree_element", Integer 1] ->
            Just $ GbTreeElement True
        Tuple [Atom _ "gb_tree_element", Integer 2] ->
            Just $ GbTreeElement False
        Atom _ "array_element" -> Just ArrayElement
        Tuple [Atom _ "map_element", key] ->
            MapElement <$> fromTerm key
        _ -> Nothing

instance FromTerm Path where
    fromTerm t = case t of
        List elems _ -> MkPath <$> mapM fromTerm elems
        _            -> Nothing


entryFromTerm :: Term -> Except String (ErlType, Path)
entryFromTerm (Tuple [t1, t2]) = do
   erlTy <- except $ maybeToEither ("Not an erlang type: " ++ show t1) (fromTerm t1)
   path <- except $ maybeToEither ("Not a path: " ++ show t2) (fromTerm t2)
   return (erlTy, path)
entryFromTerm t = throwE $ "Not an entry: " ++ show t

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _def (Just val) = Right val
maybeToEither def Nothing     = Left def



newtype TyEnv = MkTyEnv { unTyEnv :: Map FunName ErlType }
instance Show TyEnv where
    show (MkTyEnv m) =
        intercalate "\n" (map showFun $ Map.toList m) ++ "\n" where

        showFun (MkFunName{funName}, EFun ts t) = Text.unpack funName ++ "(" ++ intercalate ", " (map show ts) ++ ") -> " ++ show t
        showFun (MkFunName{funName,funArity}, _t) = Text.unpack funName ++ "/" ++ show funArity ++  ": no text available"

-- Combine, may introduce toplevel unions
combine :: ErlType -> ErlType -> ErlType
combine t1 t2 | Just t3 <- equate t1 t2 = t3
combine (ENamedAtom a1) (ENamedAtom a2)
    | a1 `elem` ["true", "false"] &&
      a2 `elem` ["true", "false"] = EBoolean
-- no support for shapemaps!
-- this is not the best, there could be too many different key types
combine (EMap m1) (EMap m2) = EMap $ Map.unionWith combine m1 m2
combine (EUnion ts) t1 | HashSet.null ts = t1
combine t1 (EUnion ts) | HashSet.null ts = t1
combine (EUnion ts1) (EUnion ts2) = mkFlatUnion $ HashSet.foldl' combineUnion ts1 ts2 -- (flip combineUnion)
combine (EUnion ts) t1 = mkFlatUnion $ combineUnion ts t1
combine t1 (EUnion ts) = mkFlatUnion $ combineUnion ts t1
combine t1 t2 = mkFlatUnion $ combineUnion (HashSet.singleton t1) t2

-- todo: better performance??
-- accumulation needs to be faster
combineUnion :: HashSet ErlType -> ErlType -> HashSet ErlType
combineUnion ts t = fromMaybe (HashSet.insert t ts) (equateUnion ts t)

equateUnion :: HashSet ErlType -> ErlType -> Maybe (HashSet ErlType)
equateUnion ts t = if didEquate then Just newTs else Nothing where
    (newTs, didEquate) = HashSet.foldl' go (HashSet.empty, False) $ HashSet.foldl' elements HashSet.empty ts

    elements set (EUnion ts') = ts' <> set
    elements set t'           = HashSet.insert t' set

    go (ts', combined') t' =
        case equate t t' of
            Just t'' -> (HashSet.insert t'' ts', True)
            Nothing  -> (HashSet.insert t' ts', combined')

-- Combine, returns Nothing instead of toplevel unions or upcasts
equate :: ErlType -> ErlType -> Maybe ErlType
equate t1 t2 | t1 == t2 = Just t1
equate EUnknown t1 = Just t1
equate t1 EUnknown = Just t1
equate _ EAny = Just EAny
equate EAny _ = Just EAny
equate (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
    Just $ EFun (zipWith combine argts1 argts2) (combine t1 t2)
equate t1@(ENamedAtom _) EAnyAtom = Just t1
equate EAnyAtom t1@(ENamedAtom _) = Just t1
equate EBoolean EAnyAtom = Just EAnyAtom
equate EAnyAtom EBoolean = Just EAnyAtom
equate (ENamedAtom txt) EBoolean | txt == "true" || txt == "false" = Just EBoolean
equate EBoolean (ENamedAtom txt) | txt == "true" || txt == "false" = Just EBoolean
equate EBitString EBinary = Just EBitString
equate EBinary EBitString = Just EBitString
-- TODO: when equating, transform inner representations to container
equate (EContainer c1) (EContainer c2) = EContainer <$> equateCont c1 c2
-- what is with record like structures?,
equate (ETuple ts1) (ETuple ts2) | length ts1 /= length ts2 = Nothing
equate (ETuple (ENamedAtom a1 : ts1)) (ETuple (ENamedAtom a2 : ts2)) | a1 == a2 =
    Just $ ETuple $ ENamedAtom a1 : zipWith combine ts1 ts2
-- should this be combine instead? don't think so
equate (ETuple ts1) (ETuple ts2) =
    ETuple <$> zipWithM equate ts1 ts2
equate (EUnion ts1) (EUnion ts2) = mkFlatUnion <$> foldM equateUnion ts1 ts2 -- (flip combineUnion)
equate (EUnion ts) t1 = mkFlatUnion <$> equateUnion ts t1
equate t1 (EUnion ts) = mkFlatUnion <$> equateUnion ts t1
--equate (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
--    Just $ EFun (zipWith combine argts1 argts2) (combine t1 t2)
equate _ _ = Nothing

-- should this rather be combine??
equateCont :: Container ErlType -> Container ErlType -> Maybe (Container ErlType)
equateCont (CList t1) (CList t2)     = Just $ CList $ combine t1 t2
equateCont (CDict t1) (CDict t2)     = Just $ CDict $ combine t1 t2
equateCont (COldSet t1) (COldSet t2) = Just $ COldSet $ combine t1 t2
equateCont (CGbSet t1) (CGbSet t2)   = Just $ CGbSet $ combine t1 t2
equateCont (CGbTree t1 t2) (CGbTree t3 t4) = Just $ CGbTree (combine t1 t3) (combine t2 t4)
equateCont CGb CGb = Just CGb
equateCont CGb (CGbSet t1) = Just $ CGbSet t1
equateCont (CGbSet t1) CGb = Just $ CGbSet t1
equateCont CGb (CGbTree t1 t2) = Just $ CGbTree t1 t2
equateCont (CGbTree t1 t2) CGb = Just $ CGbTree t1 t2
equateCont (CArray t1) (CArray t2) = Just $ CArray $ combine t1 t2
equateCont _ _ = Nothing

-- Function used to tame too large types, collaping them.
-- Not sure if the handling of Unknown values here is correct
-- should it be promoted to any or unknown or ignored?
-- float/integer --> num?
lub :: ErlType -> ErlType -> ErlType
lub t1 t2 | Just t3 <- equate t1 t2 = t3
lub ENone t2 = t2
lub t1 ENone = t1
lub (ETuple ts1) (ETuple ts2)| length ts1 == length ts2 =
    ETuple $ zipWith lub ts1 ts2
lub (EContainer c1) (EContainer c2) = lubCont c1 c2
-- TODO: actual lub for maps
lub (EMap _) (EMap _) =
    EMap $ Map.singleton EAny EAny
-- TODO: is there a need for greatest lower bound here?
-- what exactly is the type hierarchy
lub (EFun argts1 t1) (EFun argts2 t2) | length argts1 == length argts2 =
    EFun (replicate (length argts1) ENone) (lub t1 t2)
lub _ _ = EAny

lubCont :: Container ErlType -> Container ErlType -> ErlType
lubCont (CList t1) (CList t2)     = EContainer $ CList $ lub t1 t2
lubCont (CDict t1) (CDict t2)     = EContainer $ CDict $ lub t1 t2
lubCont (COldSet t1) (COldSet t2) = EContainer $ COldSet $ lub t1 t2
lubCont (CGbSet t1) (CGbSet t2)   = EContainer $ CGbSet $ lub t1 t2
lubCont (CGbTree t1 t2) (CGbTree t3 t4) = EContainer $ CGbTree (lub t1 t3) (lub t2 t4)
lubCont CGb CGb = EContainer CGb
lubCont CGb (CGbSet t1) = EContainer $ CGbSet t1
lubCont (CGbSet t1) CGb = EContainer $ CGbSet t1
lubCont CGb (CGbTree t1 t2) = EContainer $ CGbTree t1 t2
lubCont (CGbTree t1 t2) CGb = EContainer $ CGbTree t1 t2
lubCont (CArray t1) (CArray t2) = EContainer $ CArray $ lub t1 t2
lubCont _ _ = EAny


mkFlatUnion :: HashSet ErlType -> ErlType
mkFlatUnion ts | HashSet.size flatUnion > 1000 = HashSet.foldl' lub ENone flatUnion
               | otherwise = EUnion flatUnion where
--    flatUnion = squashUnionElements $ Set.fold go Set.empty ts
    flatUnion = HashSet.foldl' go HashSet.empty ts


    go set (EUnion ts') = ts' <> set
    go set t'           = HashSet.insert t' set

makeArgs :: ErlType -> Int -> Int -> [ErlType]
makeArgs t index size =
    [ getT i | i <- [1 .. size] ] where
        getT i | i == index = t
               | otherwise = EUnknown

update :: ErlType -> Path -> TyEnv -> TyEnv
update ty (MkPath p) env =
    case p of
        -- Not a record-like tuple, use first element's type
        Rec _ 1 size : p' ->
            update (ETuple $ ty : replicate (size-1) EUnknown)
                   (MkPath p') env
        Rec k index size : p' ->
            update (ETuple $ k : makeArgs ty (index-1) (size-1))
                   (MkPath p') env
        Dom pos arity : p' ->
            update (EFun (makeArgs ty pos arity) EUnknown) (MkPath p') env
        Range arity : p' ->
            update (EFun (replicate arity EUnknown) ty) (MkPath p') env
        ListElement : p' ->
            update (EContainer $ CList ty) (MkPath p') env
        DictElement : p' ->
            update (EContainer $ CDict ty) (MkPath p') env
        SetElement : p' ->
            update (EContainer $ COldSet ty) (MkPath p') env
        GbSetElement : p' ->
            update (EContainer $ CGbSet ty) (MkPath p') env
        ArrayElement : p' ->
            update (EContainer $ CArray ty) (MkPath p') env
        GbTreeElement True : p' ->
            update (EContainer $ CGbTree ty EUnknown) (MkPath p') env
        GbTreeElement False : p' ->
            update (EContainer $ CGbTree EUnknown ty) (MkPath p') env
        MapElement k : p' ->
            update (EMap $ Map.singleton k ty) (MkPath p') env
        [FunN fn] -> let tenv = unTyEnv env in
            case Map.lookup fn tenv of
                Just ty' -> MkTyEnv $ Map.insert fn (combine ty ty') tenv
                Nothing  -> MkTyEnv $ Map.insert fn ty tenv
        _ -> error "Internal error"


data AliasEnv = MkAliasEnv
    { aliasMap  :: IntMap ErlType
    , nextIndex :: !Int
    } deriving (Eq, Ord)

instance Show AliasEnv where
    show MkAliasEnv{..} =
        intercalate "\n" (map (\(i, t) -> "$" ++ show i ++ " -> " ++ show t) $ IntMap.toList aliasMap) ++
        "\n"

-- not good: throw error!
lookupAlias :: Int -> SquashConfig -> ErlType
lookupAlias i SquashConfig{aliasEnv=MkAliasEnv{..}} = aliasMap IntMap.! i

data Strategy = S1 | S2 | S3
    deriving(Eq, Ord, Show, Read)

data Options = Options
    { strategy          :: Strategy
    , upcastMixedAtoms  :: Bool
    , upcastMixedTuples :: Bool
    , atomUnionSize     :: Int
    , tupleUnionSize    :: Int
    , recordSize        :: Int
    , printUnformatted  :: Bool
    , parametersPath    :: Maybe String
    , prettyOutputPath  :: String
    , inputPath         :: String
    } deriving(Eq, Ord, Show)

data SquashConfig = SquashConfig
                  { aliasEnv :: AliasEnv
                  , tyEnv    :: TyEnv
                  , options  :: Options
                  } deriving(Show)

addFunction :: FunName -> ErlType -> SquashConfig -> SquashConfig
addFunction fn t SquashConfig{tyEnv = MkTyEnv{..},..} =
    SquashConfig{tyEnv=MkTyEnv (Map.insert fn t unTyEnv), ..}


addAlias :: Int -> ErlType -> SquashConfig -> SquashConfig
addAlias i t SquashConfig{aliasEnv=MkAliasEnv{..},..} =
    SquashConfig{aliasEnv=MkAliasEnv (IntMap.insert i t aliasMap) nextIndex, ..}


reg :: SquashConfig -> ErlType -> (SquashConfig, ErlType)
reg SquashConfig{aliasEnv = MkAliasEnv{..}, ..} t =
    (SquashConfig{aliasEnv = MkAliasEnv (IntMap.insert nextIndex t aliasMap) (nextIndex + 1), ..}, EAliasMeta nextIndex)

-- maybe do a check that we are not entering a loop, just in case
resolve :: SquashConfig -> ErlType -> ErlType
resolve conf t = go 0 t where
    go :: Int -> ErlType -> ErlType
    go n _ | n > 1000 = t
    go n (EAliasMeta i) = go (n + 1) (lookupAlias i conf)
    go _ t' = t'

aliases :: ErlType -> IntSet
aliases = para visit where
    visit :: ErlType -> [IntSet] -> IntSet
    visit (EAliasMeta i) is = IntSet.insert i $ mconcat is
    visit _ is              = mconcat is


mergeAliases :: SquashConfig -> [Int] -> SquashConfig
mergeAliases conf [] = conf
mergeAliases conf [_] = conf
mergeAliases conf as@(a1:rest) =
    mapAliasesTo (mapAliasesTo conf rest (EAliasMeta a1)) [a1] sigma where
        sigma = foldl1 combine $ map process as
        -- resolve alias -> remove top-level aliases -> change a_2..a_n refs to a_1
        -- we subst with rest, just in case there are references between a_n and and a_m, n and m > 1
        process ai =
            -- bit different from the paper: it only sets ai to a1
            substTy rest (EAliasMeta a1) (erase (resolve conf (EAliasMeta ai)))
        -- remove top-level aliases to avoid infinite types
        erase (EAliasMeta a') | a' `elem` as = EUnion HashSet.empty
        erase (EUnion ts) = mkFlatUnion $ HashSet.map erase ts
        erase t = t

-- -- | Change all occurences of aliases to newT in a type
substTy :: [Int] -> ErlType -> ErlType -> ErlType
substTy as newT = transform go where
    go (EAliasMeta alias) | alias `elem` as = newT
    go t = t

substTy' :: IntMap ErlType -> ErlType -> ErlType
substTy' sub = transform go where
    go (EAliasMeta alias) =
        case IntMap.lookup alias sub of
            Just t -> t
            _      -> EAliasMeta alias
    go t = t

mapAliasesTo :: SquashConfig -> [Int] -> ErlType -> SquashConfig
mapAliasesTo SquashConfig{aliasEnv=MkAliasEnv{..},..} as newT =
    SquashConfig{aliasEnv=MkAliasEnv{aliasMap=newAliases, ..}, ..} where
        newAliases :: IntMap ErlType
        newAliases =
            IntMap.mapWithKey
                (\i t -> if i `elem` as then newT else t)
                aliasMap

-- IMPORTANT! UPDATE WHEN ErlType changes
postwalk :: SquashConfig -> ErlType ->
             (SquashConfig -> ErlType -> (SquashConfig, ErlType)) ->
             (SquashConfig, ErlType)
postwalk conf_ t_ f = postwalk' conf_ t_ where
    postwalk' conf t = case t of
        ETuple ts ->
            let (conf', ts') = foldChildren ts in f conf' (ETuple ts')
        -- inefficient, is hashset the correct choice here?
        EUnion hs ->
            let (conf', hs') = foldChildren (HashSet.toList hs) in f conf' (EUnion $ HashSet.fromList hs')
        EFun args ret ->
            let (conf', args') = foldChildren args
                (conf'', ret') = postwalk' conf' ret
            in
                f conf'' (EFun args' ret')
        EMap m ->
            -- TODO: handle maps
            (conf, EMap m)
        EContainer c -> doCont c
        _ -> f conf t
        where
        foldChildren = foldr go (conf, mempty)
        go t' (conf', ts') =
            let (conf'', t'') = postwalk' conf' t' in
            (conf'', t'':ts')

        doCont (CList t1) =
            let (conf', t1') = postwalk' conf t1 in f conf' (EContainer $ CList t1')
        doCont (CDict t1) =
            let (conf', t1') = postwalk' conf t1 in f conf' (EContainer $ CDict t1')
        doCont (COldSet t1) =
            let (conf', t1') = postwalk' conf t1 in f conf' (EContainer $ COldSet t1')
        doCont (CGbSet t1) =
            let (conf', t1') = postwalk' conf t1 in f conf' (EContainer $ CGbSet t1')
        doCont (CArray t1) =
            let (conf', t1') = postwalk' conf t1 in f conf' (EContainer $ CArray t1')
        doCont CGb = f conf (EContainer CGb)
        doCont (CGbTree t1 t2) =
            let (conf', t1') = postwalk' conf t1
                (conf'', t2') = postwalk' conf' t2
            in
                f conf'' (EContainer $ CGbTree t1' t2')

