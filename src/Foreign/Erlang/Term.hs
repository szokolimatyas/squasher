{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Foreign.Erlang.Term(ExternalTerm(..), Term(..), FromTerm(..)) where

import           Control.Monad      (replicateM)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.Bits          ()
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS (foldr', length, take)
import           Data.Int
import qualified Data.List          as L
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Enc
import           GHC.Generics
import           GHC.Stack          (HasCallStack)

newtype ExternalTerm = MkExternalTerm {fromExternalTerm :: Term}
  deriving (Eq, Generic, Show)

instance Binary ExternalTerm where
  put (MkExternalTerm t) = do
    putWord8 magicVersion
    put t
  get = do
    matchWord8 magicVersion
    MkExternalTerm <$> get
data Term
  = Integer Integer
  | Float Double
  | Atom AtomType Text
  | Reference AtomType Text Word32 Word8
  | NewReference AtomType Text Word8 [Word32]
  | NewerReference AtomType Text Word32 [Word32]
  | Port AtomType Text Word32 Word8
  | NewPort AtomType Text Word32 Word32
  | Pid AtomType Text Word32 Word32 Word8
  | NewPid AtomType Text Word32 Word32 Word32
  | Tuple [Term]
  | Map [MapEntry]
  | Nil
  | String Text
  | List [Term] Term
  | Binary ByteString
  deriving (Eq, Generic, Show)


data AtomType
  = OldAtom
  | OldSmallAtom
  | AtomUtf8
  | SmallAtomUtf8
  deriving (Eq, Generic, Ord, Bounded, Enum, Show)


data MapEntry = MapEntry
  { key   :: Term,
    value :: Term
  }
  deriving(Eq, Generic, Show)

class FromTerm a where
    fromTerm :: Term -> Maybe a

--------------------------------------------------------------------------------
-- taken from hinterface
-- TODO: license?
magicVersion :: Word8
magicVersion = 131

smallIntegerExt,
  integerExt,
--  floatExt,
  atomExt,
  referenceExt,
  newReferenceExt,
  newerReferenceExt,
  portExt,
  newPortExt,
  newPidExt,
  pidExt,
  smallTupleExt,
  largeTupleExt,
  mapExt,
  nilExt,
  stringExt,
  listExt,
  binaryExt,
  -- newFunctionExt,
  smallBigIntegerExt,
  largeBigIntegerExt,
  smallAtomExt,
  -- functionExt,
  -- exportExt,
  -- bitBinaryExt,
  newFloatExt,
  atomUtf8Ext,
  smallAtomUtf8Ext ::
    Word8
smallIntegerExt = 97
integerExt = 98
-- floatExt = 99
atomExt = 100
smallAtomExt = 115
atomUtf8Ext = 118
smallAtomUtf8Ext = 119
referenceExt = 101
newReferenceExt = 114
newerReferenceExt = 90
portExt = 102
newPortExt = 89
pidExt = 103
newPidExt = 88
smallTupleExt = 104
largeTupleExt = 105
mapExt = 116
nilExt = 106
stringExt = 107
listExt = 108
binaryExt = 109
smallBigIntegerExt = 110
largeBigIntegerExt = 111
-- functionExt = 117
-- newFunctionExt = 112
-- exportExt = 113
-- bitBinaryExt = 77
newFloatExt = 70


instance Binary MapEntry where
  put MapEntry {key, value} = do
    put key
    put value
  get = MapEntry <$> get <*> get

instance Binary Term where
  put (Integer i)
    | i >= 0x00 && i <= 0xFF = do
      putWord8 smallIntegerExt
      putWord8 (fromIntegral i)
    | i >= -0x80000000 && i <= 0x7FFFFFFF = do
      putWord8 integerExt
      putWord32be (fromIntegral i)
    | otherwise =
      -- NOTE: the biggest number presentable is 2^maxBits bits long where
      -- maxBits = 2^32 * 8 = 2^35 - OTOH addressable main memory: 2^64 *
      -- 8 bits = 2^67 bits, even with tomorrows 2048 bit address buses
      -- for 256 bit words this would be at most 2^2056 addressable bits.
      -- largeBigIntegerExt allows 2^(2^35) = 2^34359738368 addressable bits ..
      -- hence YES by all practical means 'otherwise' is the correct
      -- function clause guard.
      do
        let digits = L.unfoldr takeLSB (abs i)
              where
                takeLSB x
                  | x == 0 = Nothing
                  | otherwise = Just (fromIntegral (x Data.Bits..&. 0xff), x `shiftR` 8)
        if L.length digits < 256
          then do
            putWord8 smallBigIntegerExt
            putWord8 (fromIntegral (L.length digits))
          else do
            putWord8 largeBigIntegerExt
            putWord32be (fromIntegral (L.length digits))
        putWord8 (if i >= 0 then 0 else 1)
        mapM_ putWord8 digits
  put (Float d) = do
    putWord8 newFloatExt
    putDoublebe d
  put (Atom OldAtom n) = do
    putWord8 atomExt
    putLength16beText n
  put (Atom OldSmallAtom n) = do
    putWord8 smallAtomExt
    putLength8Text n
  put (Atom AtomUtf8 n) = do
    putWord8 atomUtf8Ext
    putLength16beText n
  put (Atom SmallAtomUtf8 n) = do
    putWord8 smallAtomUtf8Ext
    putLength8Text n
  put (Reference nt node id creation) = do
    putWord8 referenceExt
    put (Atom nt node)
    putWord32be id
    putWord8 creation
  put (NewReference nt node creation ids) = do
    putWord8 newReferenceExt
    putWord16be $ fromIntegral (L.length ids)
    put (Atom nt node)
    putWord8 creation
    mapM_ putWord32be ids
  put (NewerReference nt node creation ids) = do
    putWord8 newerReferenceExt
    putWord16be $ fromIntegral (L.length ids)
    put (Atom nt node)
    putWord32be creation
    mapM_ putWord32be ids
  put (Port nt node id creation) = do
    putWord8 portExt
    put (Atom nt node)
    putWord32be id
    putWord8 creation
  put (NewPort nt node id creation) = do
    putWord8 newPortExt
    put (Atom nt node)
    putWord32be id
    putWord32be creation
  put (Pid nt node id serial creation) = do
    putWord8 pidExt
    put (Atom nt node)
    putWord32be id
    putWord32be serial
    putWord8 creation
  put (NewPid nt node id serial creation) = do
    putWord8 newPidExt
    put (Atom nt node)
    putWord32be id
    putWord32be serial
    putWord32be creation
  put (Tuple v)
    | length v < 256 = do
      putWord8 smallTupleExt
      putWord8 $ fromIntegral (length v)
      mapM_ put v
    | otherwise = do
      putWord8 largeTupleExt
      putWord32be $ fromIntegral (length v)
      mapM_ put v
  put (Map e) = do
    putWord8 mapExt
    putWord32be $ fromIntegral (length e)
    mapM_ put e
  put Nil = putWord8 nilExt
  put (String s) = do
    putWord8 stringExt
    putLength16beText s
  put (List v t) = do
    putWord8 listExt
    putWord32be $ fromIntegral (length v)
    mapM_ put v
    put t
  put (Binary b) = do
    putWord8 binaryExt
    putLength32beByteString b

  get = getWord8 >>= get'
    where
      get' :: Word8 -> Get Term
      get' tag
        | tag == smallIntegerExt = Integer . fromIntegral <$> getWord8
        | tag == integerExt = Integer . toInteger . (fromIntegral :: Word32 -> Int32) <$> getWord32be
        | tag == smallBigIntegerExt = getWord8 >>= getBigInteger . fromIntegral
        | tag == largeBigIntegerExt = getWord32be >>= getBigInteger . fromIntegral
        | tag == newFloatExt = Float <$> getDoublebe
        | tag == atomExt = Atom OldAtom <$> getLength16beText
        | tag == smallAtomExt = Atom OldSmallAtom <$> getLength8Text
        | tag == atomUtf8Ext = Atom AtomUtf8 <$> getLength16beText
        | tag == smallAtomUtf8Ext = Atom SmallAtomUtf8 <$> getLength8Text
        | tag == portExt = uncurry Port <$> getNodeNameAtom <*> getWord32be <*> getWord8
        | tag == newPortExt = uncurry NewPort <$> getNodeNameAtom <*> getWord32be <*> getWord32be
        | tag == pidExt = uncurry Pid <$> getNodeNameAtom <*> getWord32be <*> getWord32be <*> getWord8
        | tag == newPidExt = uncurry NewPid <$> getNodeNameAtom <*> getWord32be <*> getWord32be <*> getWord32be
        | tag == smallTupleExt = Tuple <$> (getWord8 >>= getList . fromIntegral)
        | tag == largeTupleExt = Tuple <$> (getWord32be >>= getList . fromIntegral)
        | tag == mapExt = Map <$> (getWord32be >>= getList . fromIntegral)
        | tag == nilExt = pure Nil
        | tag == stringExt = String <$> getLength16beText
        | tag == listExt = List <$> (getWord32be >>= getList . fromIntegral) <*> get
        | tag == binaryExt = Binary <$> getLength32beByteString
        | tag == referenceExt =
          uncurry Reference <$> getNodeNameAtom <*> getWord32be <*> getWord8
        | tag == newReferenceExt = do
          len <- getWord16be
          (nt, nn) <- getNodeNameAtom
          NewReference nt nn <$> getWord8 <*> getList (fromIntegral len)
        | tag == newerReferenceExt = do
          len <- getWord16be
          uncurry NewerReference <$> getNodeNameAtom <*> getWord32be <*> getList (fromIntegral len)
        | otherwise = fail $ "Unsupported tag: " ++ show tag

getNodeNameAtom :: Get (AtomType, Text)
getNodeNameAtom =
  get >>= \case
    Atom nt nn -> return (nt, nn)
    other      -> fail ("expected a node name atom, but got: " ++ show other)

getList :: HasCallStack => Binary a => Int -> Get [a]
getList len = replicateM len get

getBigInteger :: HasCallStack => Int -> Get Term
getBigInteger len = mkBigInteger <$> getWord8 <*> getByteString len
  where
    mkBigInteger signByte bs =
      Integer
        ((if signByte == 0 then 1 else (-1)) * absInt)
      where
        absInt = BS.foldr' (\b acc -> 256 * acc + fromIntegral b) 0 bs

getLength16beText :: HasCallStack => Get Text
getLength16beText =
  Enc.decodeUtf8 <$> (getWord16be >>= getByteString . fromIntegral)

getLength8Text :: HasCallStack => Get Text
getLength8Text = Enc.decodeUtf8 <$> (getWord8 >>= getByteString . fromIntegral)

getLength32beByteString :: HasCallStack => Get ByteString
getLength32beByteString =
  getWord32be >>= getByteString . fromIntegral

putLength8Text :: HasCallStack => Text -> Put
putLength8Text = putLength8ByteString . Enc.encodeUtf8 . Text.take 255

putLength16beText :: HasCallStack => Text -> Put
putLength16beText = putLength16beByteString . Enc.encodeUtf8 . Text.take 65535

putLength8ByteString :: HasCallStack => ByteString -> Put
putLength8ByteString bs = do
  putWord8 (fromIntegral (BS.length bs))
  putByteString (BS.take 255 bs)

putLength16beByteString :: HasCallStack => ByteString -> Put
putLength16beByteString bs = do
  putWord16be (fromIntegral (BS.length bs))
  putByteString (BS.take 65535 bs)

putLength32beByteString :: HasCallStack => ByteString -> Put
putLength32beByteString bs = do
  putWord32be (fromIntegral (BS.length bs))
  putByteString bs

matchWord8 :: HasCallStack => Word8 -> Get ()
matchWord8 expected = do
  actual <- getWord8
  if expected == actual then return () else fail $ "expected " ++ show expected ++ ", actual " ++ show actual
