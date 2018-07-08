{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheme.Program where

import Scheme.Types
import Scheme.References
import Scheme.CodeStruct
import Scheme.AST

genCode :: AST -> CodeStruct
genCode =
  compileToCodeStruct . resolveReferences 

testProgram :: AST
testProgram = 
  List [ ASymbol "begin"
       , List [ ASymbol "define"
              , ASymbol "x"
              , Atom (AInt 10)
              ]
       , List [ ASymbol "print"
              , Atom (AString "hey")
              , ASymbol "x"
              ]
       ]
                
