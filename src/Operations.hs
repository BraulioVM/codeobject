module Operations where

import qualified Data.ByteString as BS
import Data.Bits
import Data.ByteString (ByteString, pack, singleton)
import Data.Word

data Operation = BINARY_ADD
               | BINARY_MULTIPLY
               | LOAD_FAST Word16
               | STORE_FAST Word16
               | LOAD_CONSTANT Word16
               | PRINT_EXPR
               | RETURN_VALUE
  
getByteCode :: [Operation] -> ByteString
getByteCode = BS.concat . fmap opByteCode

opByteCode :: Operation -> ByteString
opByteCode BINARY_ADD = singleton 23
opByteCode BINARY_MULTIPLY = singleton 20
opByteCode (LOAD_FAST n) = pack [124, byte2, byte1]
  where
    [byte1, byte2] = encodeWord16 n
opByteCode (STORE_FAST n) = pack [125, byte2, byte1]
  where
    [byte1, byte2] = encodeWord16 n

opByteCode (LOAD_CONSTANT n) = pack [100, byte1, byte2]
  where
    [byte1, byte2] = encodeWord16 n

opByteCode RETURN_VALUE = singleton 83
opByteCode PRINT_EXPR = singleton 70


encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [
  x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8
  ]
