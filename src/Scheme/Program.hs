{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scheme.Program where

import Scheme.Types
import Scheme.References
import Scheme.CodeStruct
import Scheme.AST
import Scheme.Parser

astToCodeStruct :: AST -> Either CompileError CodeStruct
astToCodeStruct ast =
  parseStandardForms ast >>= resolveReferences >>= compileToCodeStruct

testProgram :: AST
testProgram = 
  List [ ASymbol "begin"
       , List [ ASymbol "define"
              , ASymbol "x"
              , Atom (AInt 10)
              ]
       , List [ ASymbol "define"
              , ASymbol "print"
              , ASymbol "x"
              ]
       -- , List [ ASymbol "print"
       --        , Atom (AString "hey")
       --        , ASymbol "x"
       --        ]
       ]
                
