module ResolvedProgramTests where

import qualified Data.Map as Map

import Test.HUnit

import Scheme.References.Internal
import Scheme.References
import Scheme.Types
import Scheme.AST

testNestedLambdas :: Test
testNestedLambdas = TestCase $ do
  assertEqual "resolved program" (resolveReferences program) resolved

  where
    program :: FAST
    program =
      FLambda ["x", "y"] (
      FBegin [ FDefine "z" (FAtom (AInt 5))
             , FDefine "local" (FAtom (AInt 6))
             , FLambda ["_", "_"] (
                 FBegin
                   [ FDefine "l" (FReference "z")
                   , FLambda ["_", "_"] (FDefine "y" (FReference "l"))
                   ]
                 )
             ]
      )

    resolved = Right
      (Scope 
        { scopeCode = FAtom (ConstantVarReference 0)
        , scopeConstants =
            [ Left $ Scope
              { scopeCode =
                  FBegin
                  [ FDefine "z"
                    (FAtom $ ConstantVarReference 0)
                  , FDefine "local"
                    (FAtom $ ConstantVarReference 1)
                  , FAtom (ConstantVarReference 2)
                  ]
              , scopeConstants =
                [ Right (AInt 5)
                , Right (AInt 6)
                , Left $ Scope
                  { scopeCode =
                      FBegin
                      [ FDefine "l"
                        (FReference "z")
                      , FAtom (ConstantVarReference 0)
                      ]
                  , scopeConstants =
                    [
                      Left $ Scope
                      { scopeCode =
                          FDefine "y"
                          (FReference "l")
                      , scopeConstants = []
                      , scopeVars = Map.fromList
                                    [ ("y", LocalScope)
                                    , ("l", FreeScope)
                                    ]
                      }
                    ]
                  , scopeVars = Map.fromList
                                [ ("l", CellScope)
                                , ("z", FreeScope)
                                ]
                  }
                ]
              , scopeVars = Map.fromList
                            [ ("local", LocalScope)
                            , ("z", CellScope)
                            ]
              }
            ]
        , scopeVars = Map.empty
        }
      )

tests :: Test
tests = TestList
  [ TestLabel "nested lambdas test" testNestedLambdas ]
