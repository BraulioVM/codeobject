{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Marshal where

import Data.Bits
import Data.ByteString (pack, singleton, ByteString)
import Data.Binary (encode, Binary)
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Types

class Marshable m where
  marshal :: m -> ByteString

csingleton :: Char -> ByteString
csingleton = BSC.singleton

enc :: (Binary a) => a -> ByteString
enc = BS.concat . BL.toChunks . encode

encLong :: Int -> ByteString
encLong = enc . byteSwap32 . fromIntegral 

instance Marshable Int where
  marshal x = BS.concat [
    csingleton 'i',
    enc x
    ]

instance Marshable ByteString where
  marshal s = csingleton 's' `BS.append`
              encLong (BS.length s) `BS.append`
              s

instance Marshable PExpr where
  marshal PNone = csingleton 'N'
  marshal (PInt x) = csingleton 'i' `BS.append`
                     encLong x

instance Marshable String where
  marshal s = csingleton 'a' `BS.append`
              encLong (length s) `BS.append`
              (BSC.pack s)

instance Marshable a => Marshable (PTuple a) where
  marshal (PTuple as) = 
    csingleton '(' `BS.append` -- long tuple uses int for size
        encLong n `BS.append`  -- small tuple uses just one byte
        BS.concat (marshal <$> as)
    where
      n = length as

instance Marshable CodeObject where
  marshal obj = BS.concat (fmap ($ obj) [
    const (singleton $ 0x63 .|. 0x80),
    encLong . argCount,
    encLong . kwOnlyArgCount,
    encLong . nLocals,
    encLong . stackSize,
    encLong . flags,
    marshal . codeString,
    marshal . constants,
    marshal . names,
    marshal . varNames,
    marshal . freeVars,
    marshal . cellVars,
    marshal . filename,
    marshal . name,
    encLong . firstLineNo,
    marshal . lnotab
    ])

data PycFile = PycFile CodeObject

instance Marshable PycFile where
  marshal (PycFile co) = BS.concat [
      pack [0x16, 0x0d, 0x0d, 0x0a] -- magic number python35
    , encLong 0x5b337342 -- timestamp
    , encLong 0 -- source size
    , marshal co
    ]

writePycFile :: FilePath -> PycFile -> IO ()
writePycFile fp pyc = BS.writeFile fp (marshal pyc)
