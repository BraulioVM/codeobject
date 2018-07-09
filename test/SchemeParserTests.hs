module SchemeParserTests where

import Test.HUnit

import Scheme.Types
import Scheme.Parser


testDefine :: Test
testDefine = TestCase $
  assertEqual "standard forms" (parseStandardForms ast) standardForms
  where
    ast = List [ ASymbol "begin"
               , List [ ASymbol "define"
                      , ASymbol "x"
                      , Atom (AInt 10)
                      ]
               , List [ ASymbol "define"
                      , ASymbol "y"
                      , Atom (AString "hola")
                      ]
               ]

    standardForms = Right $ FBegin
      [ FDefine "x" (FAtom (AInt 10))
      , FDefine "y" (FAtom (AString "hola"))
      ]

testFApply :: Test
testFApply = TestCase $
  assertEqual "parsed form" (parseStandardForms ast) standardForms
  where
    ast = List [ ASymbol "f" ]
    standardForms = Right $ FApply "f" []

testLambda :: Test
testLambda = TestCase $
  assertEqual "parsed form" (parseStandardForms ast) standardForms
  where
    ast = List [ ASymbol "lambda"
               , List [ ASymbol "x"
                      , ASymbol "y"
                      ]
               , List [ ASymbol "f"
                      , ASymbol "x"
                      , ASymbol "y"
                      ]
               ]
    standardForms = Right $ (FLambda ["x", "y"]
                              (FApply "f" [ FReference "x"
                                          , FReference "y"
                                          ]
                              )
                            )
      


testIncorrectLambda :: Test
testIncorrectLambda = TestCase $
  assertEqual "standard forms" (parseStandardForms ast) compError
  where
    ast = List [ ASymbol "lambda"
               , List [ ASymbol "x"
                      , Atom (AInt 3)
                      ]
               , List []
               ]

    compError = Left IncorrectParameterList

tests :: Test
tests = TestList
        [ TestLabel "test define" testDefine
        , TestLabel "test f apply" testFApply
        , TestLabel "test basic lambda" testLambda
        , TestLabel "test incorrect lambda" testIncorrectLambda
        ]
