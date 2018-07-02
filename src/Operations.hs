module Operations where

import qualified Data.ByteString as BS
import Data.Bits
import Data.ByteString (ByteString, pack, singleton)
import Data.Word

-- | Represents python bytecode instructions
data Operation = BINARY_ADD
               | BINARY_MULTIPLY
               | LOAD_FAST Word16
               | STORE_FAST Word16
               | LOAD_CONSTANT Word16
               | PRINT_EXPR
               | RETURN_VALUE
  
-- | Turn a string of bytecode instructions
-- into the associated bytestring
getByteCode :: [Operation] -> ByteString
getByteCode = BS.concat . fmap opByteCode

-- | Turn one instruction into its associated bytecode
opByteCode :: Operation -> ByteString
opByteCode BINARY_ADD = singleton 23
opByteCode BINARY_MULTIPLY = singleton 20
opByteCode (LOAD_FAST n) = pack [124, lsb, msb]
  where
    (lsb, msb) = encodeWord16 n
opByteCode (STORE_FAST n) = pack [125, lsb, msb]
  where
    (lsb, msb) = encodeWord16 n
opByteCode (LOAD_CONSTANT n) = pack [100, lsb, msb]
  where
    (lsb, msb) = encodeWord16 n
opByteCode RETURN_VALUE = singleton 83
opByteCode PRINT_EXPR = singleton 70

encodeWord16 :: Word16 -> (Word8, Word8)
encodeWord16 x =
  ( fromIntegral $ x .&. 0xFF
    , fromIntegral $ (x .&. 0xFF00) `shiftR` 8
  )

    
