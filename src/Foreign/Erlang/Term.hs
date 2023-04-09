module Foreign.Erlang.Term where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString as BS
  ( foldr',
    head,
    length,
    tail,
    unpack,
  )
import Data.Word (Word8)

data Term
  = Integer Integer
  | Float Double
  | Atom AtomType Text
  | Reference 
  | Port 
  | Pid
  | Tuple [Term]
  | Map [MapEntry]
  | Nil
  | String Text
  | List [Term] Term
  | Binary ByteString
  deriving (Eq, Show)

data AtomType
  = OldAtom
  | OldSmallAtom
  | AtomUtf8
  | SmallAtomUtf8
  deriving (Eq, Ord, Show)
    

data MapEntry = MapEntry
  { key :: Term,
    value :: Term
  }
  deriving(Eq, Show)


--------------------------------------------------------------------------------
-- taken from hinterface
-- TODO: license?
magicVersion :: Word8
magicVersion = 131

smallIntegerExt,
  integerExt,
  floatExt,
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
  newFunctionExt,
  smallBigIntegerExt,
  largeBigIntegerExt,
  smallAtomExt,
  functionExt,
  exportExt,
  bitBinaryExt,
  newFloatExt,
  atomUtf8Ext,
  smallAtomUtf8Ext ::
    Word8
smallIntegerExt = 97
integerExt = 98
floatExt = 99
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
functionExt = 117
newFunctionExt = 112
exportExt = 113
bitBinaryExt = 77
newFloatExt = 70