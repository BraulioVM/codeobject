module Operations where

import qualified Data.ByteString as BS
import Data.Bits
import Data.ByteString (ByteString, pack, singleton)
import Data.Word

-- | Comparison operators used along the COMPARE_OP opcode
data ComparisonOperation = LESS
                         | LESS_OR_EQUAL
                         | EQUAL
                         | NOT_EQUAL
                         | GREATER
                         | GREATER_OR_EQUAL
                         | IN
                         | NOT_IN
                         | IS
                         | IS_NOT

-- | Represents python bytecode instructions
data Operation = BINARY_ADD
               | BINARY_MULTIPLY
               | LOAD_FAST Word16
               | STORE_FAST Word16
               | LOAD_CONSTANT Word16
               | PRINT_EXPR
               | RETURN_VALUE
               | JUMP_FORWARD Word16
               | POP_JUMP_IF_TRUE Word16
               | POP_JUMP_IF_FALSE Word16
               | COMPARE_OP ComparisonOperation
  
-- | Turn a string of bytecode instructions
-- into the associated bytestring
getByteCode :: [Operation] -> ByteString
getByteCode = BS.concat . fmap opByteCode

-- | Turn one instruction into its associated bytecode
opByteCode :: Operation -> ByteString
opByteCode BINARY_ADD = singleton 23
opByteCode BINARY_MULTIPLY = singleton 20
opByteCode (LOAD_FAST n) = opWithArgument 124 n
  where
    (lsb, msb) = encodeWord16 n
opByteCode (STORE_FAST n) = opWithArgument 125 n
  where
    (lsb, msb) = encodeWord16 n
opByteCode (LOAD_CONSTANT n) = opWithArgument 100 n
  where
    (lsb, msb) = encodeWord16 n
opByteCode RETURN_VALUE = singleton 83
opByteCode PRINT_EXPR = singleton 70

opByteCode (JUMP_FORWARD n) = opWithArgument 110 n
opByteCode (POP_JUMP_IF_TRUE n) = opWithArgument 115 n
opByteCode (POP_JUMP_IF_FALSE n) = opWithArgument 114 n

opByteCode (COMPARE_OP cmp) = pack [107, cmp_code cmp, 0]
  where
    cmp_code LESS = 0
    cmp_code LESS_OR_EQUAL = 1
    cmp_code EQUAL = 2
    cmp_code NOT_EQUAL = 3
    cmp_code GREATER = 4
    cmp_code GREATER_OR_EQUAL = 5
    cmp_code IN = 6
    cmp_code NOT_IN = 7
    cmp_code IS = 8
    cmp_code IS_NOT = 9

opWithArgument :: Word8 -> Word16 -> ByteString
opWithArgument op n = pack [op, lsb, msb]
  where
    (lsb, msb) = encodeWord16 n

encodeWord16 :: Word16 -> (Word8, Word8)
encodeWord16 x =
  ( fromIntegral $ x .&. 0xFF
    , fromIntegral $ (x .&. 0xFF00) `shiftR` 8
  )

    
