{-# LANGUAGE GADTs #-}
module Scheme.Operations where

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Data.Bifunctor (first)
import Data.Either (fromLeft)
import Scheme.References.Internal
import Scheme.References.Types
import Scheme.Types
import Types
import qualified Operations as Op

data Operation = LoadVar MutableRef
               | SaveVar MutableRef
               | MakeClosure Int [MutableRef]
               | LoadConst ConstReference
               | BinaryAdd
               | Return
               | PrintExpr
               | CallFunction Int

data CodeStruct = CodeStruct
                  { csCode :: [Operation]
                  , csConstants :: [Either CodeStruct BasicValue]
                  , csFree :: [String]
                  , csCell :: [String]
                  , csLocal :: [String]
                  , csArgumentNames :: [String]
                  }

compileCS :: CodeStruct -> CodeObject
compileCS (CodeStruct code consts free cell local argNames) = CodeObject
  { argCount = length argNames
  , kwOnlyArgCount = 0
  , nLocals = numberLocals
  , stackSize = computeStackSize code
  , flags = 0x40
  , codeString = Op.getByteCode (concatMap toPyByteCode code)
  , constants = PTuple (getConstants <$> consts)
  , names = PTuple []
  , varNames = PTuple local
  , filename = ""
  , name = ""
  , firstLineNo = 0
  , lnotab = BS.empty
  , freeVars = PTuple free
  , cellVars = PTuple cell
  }

  where
    computeStackSize :: [Operation] -> Int
    computeStackSize _ = 100

    numberLocals :: Int
    numberLocals = Set.size (Set.fromList $ local ++ argNames ++ cell)
  
    getConstants :: Either CodeStruct BasicValue -> PyExpr
    getConstants (Left is) = toPyExpr (compileCS is)
    getConstants (Right value) = toPyExpr (toPyExpr value)

    toPyByteCode :: Operation -> [Op.Operation]
    toPyByteCode (LoadVar (LocalVarReference x)) =
      [Op.LOAD_FAST (fromIntegral x)]
    toPyByteCode (LoadVar (CellVarReference x)) =
      [Op.LOAD_CLOSURE (fromIntegral x)]
    toPyByteCode (LoadVar (FreeVarReference x)) =
      [Op.LOAD_CLOSURE (fromIntegral $ x + length cell)]
    toPyByteCode (SaveVar (LocalVarReference x)) =
      [Op.STORE_FAST (fromIntegral x)]
    toPyByteCode (SaveVar (CellVarReference x)) =
      [Op.STORE_DEREF (fromIntegral x)]
    toPyByteCode (SaveVar (FreeVarReference x)) =
      [Op.STORE_DEREF (fromIntegral $ x + length cell)]

    toPyByteCode (LoadConst (ConstantVarReference x)) =
      [Op.LOAD_CONST (fromIntegral x)]
    toPyByteCode Return =
      [Op.RETURN_VALUE]
    toPyByteCode (MakeClosure ident refs) =
      mappend ((Op.LOAD_CLOSURE . fromIntegral . getIndex) <$> refs) $
      [ Op.BUILD_TUPLE (fromIntegral $ length refs)
      , Op.LOAD_CONST (fromIntegral ident)
      , Op.MAKE_CLOSURE $
        fromIntegral $ 
        fromLeft 0 $ first (length . csArgumentNames) (consts !! ident)
      ]

    toPyByteCode BinaryAdd = [Op.BINARY_ADD]
    toPyByteCode PrintExpr = [Op.PRINT_EXPR]
    toPyByteCode (CallFunction n) =
      [ Op.CALL_FUNCTION $ fromIntegral n ]
