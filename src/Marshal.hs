{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Marshal where

import Data.Bits
import Data.ByteString (pack, singleton, ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.Binary (encode, Binary)
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Types

-- | Basic encoding functions

-- | Encode a single character
csingleton :: Char -> ByteString
csingleton = BSC.singleton

-- | Little endian encoding of an integer
encInt :: Int -> ByteString
encInt = enc . byteSwap32 . fromIntegral 
  where
    enc :: (Binary a) => a -> ByteString
    enc = BS.concat . BL.toChunks . encode

-- | Objects that can be serialized using
-- python's marshal format
class Marshable m where
  marshal :: m -> ByteString

instance Marshable ByteString where
  marshal s = csingleton 's' `BS.append`
              encInt (BS.length s) `BS.append`
              s

instance Marshable PyExpr where
  marshal PyNone = csingleton 'N'
  marshal (PyInt x) = csingleton 'i' `BS.append`
                     encInt x
  marshal (PyString s) = csingleton 'u' `BS.append`
                         encInt size `BS.append`
                         payload
    where
      payload = fromString s
      size = BS.length payload

  marshal (PyBool True) = csingleton 'T'
  marshal (PyBool False) = csingleton 'F'
  marshal (PyTuple exprs) = case smallSize of
    Just small -> csingleton ')' `BS.append`
                  singleton small `BS.append`
                  items
    Nothing -> csingleton '(' `BS.append`
               encInt size `BS.append`  
               items
    where
      size = length exprs
      smallSize :: Maybe Word8
      smallSize = if (0 <= size && size < 256)
                  then Just (fromIntegral size)
                  else Nothing

      items = BS.concat (marshal <$> exprs)

instance Marshable CodeObject where
  marshal obj = BS.concat (fmap ($ obj) [
    const (csingleton 'c'),
    encInt . argCount,
    encInt . kwOnlyArgCount,
    encInt . nLocals,
    encInt . stackSize,
    encInt . flags,
    marshal . codeString,
    marshal . toPyExpr . constants,
    marshal . toPyExpr . names,
    marshal . toPyExpr . varNames,
    marshal . toPyExpr . freeVars,
    marshal . toPyExpr . cellVars,
    marshal . toPyExpr . filename,
    marshal . toPyExpr . name,
    encInt . firstLineNo,
    marshal . lnotab
    ])

-- | Unix timestamp
type Timestamp = Int

-- | Represents a pyc file
data PycFile = PycFile Timestamp CodeObject

instance Marshable PycFile where
  marshal (PycFile timestamp codeObject) = BS.concat
    [ python35MagicNumber
    , encInt timestamp
    , encInt 0 -- source size
    , marshal codeObject
    ]
   where
     -- | .pyc file magic number used by
     -- python3.5
     python35MagicNumber :: ByteString
     python35MagicNumber = pack [0x16, 0x0d, 0x0d, 0x0a]

-- | Turn a `PycFile` into a real .pyc file.
writePycFile :: FilePath -> PycFile -> IO ()
writePycFile fp pyc = BS.writeFile fp (marshal pyc)
