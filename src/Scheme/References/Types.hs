{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Scheme.References.Types (AReference,
                                Reference,
                                ConstReference,
                                MutableRef,
                                NReference,
                                ReferenceType(..),
                                getIndex,
                                isLocal,
                                isCell,
                                isFree
                                ) where

import Scheme.References.Internal

isLocal :: AReference i a -> Bool
isLocal (LocalVarReference _) = True
isLocal _ = False

isCell :: AReference i a -> Bool
isCell (CellVarReference _) = True
isCell _ = False

isFree :: AReference i a -> Bool
isFree (FreeVarReference _) = True
isFree _ = False

getIndex :: AReference i a -> i
getIndex (LocalVarReference i) = i
getIndex (ConstantVarReference i) = i
getIndex (FreeVarReference i) = i
getIndex (CellVarReference i) = i

type ConstReference = Reference 'ReadOnly
type MutableRef = Reference 'Writable
