module ResolvedProgramTests where

import qualified Data.Map as Map

import Test.HUnit

import Scheme.References.Internal
import Scheme.References
import Scheme.Types
import Scheme.AST

import Types

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
      (ResolvedProgram $ Scope 
        { scopeAST = FAtom (ConstantVarReference 0)
        , scopeConstants =
            [ Right $ Scope
              { scopeAST =
                  FBegin
                  [ FDefine (CellVarReference "z")
                    (FAtom $ ConstantVarReference 0)
                  , FDefine (LocalVarReference "local")
                    (FAtom $ ConstantVarReference 1)
                  , FAtom (ConstantVarReference 2)
                  ]
              , scopeConstants =
                [ Left (PyInt 5)
                , Left (PyInt 6)
                , Right $ Scope
                  { scopeAST =
                      FBegin
                      [ FDefine (CellVarReference "l")
                        (FReference $ FreeVarReference "z")
                      , FAtom (ConstantVarReference 0)
                      ]
                  , scopeConstants =
                    [
                      Right $ Scope
                      { scopeAST =
                          FDefine (LocalVarReference "y")
                          (FReference (FreeVarReference "l"))
                      , scopeConstants = []
                      , scopeLocalVariables = Map.fromList
                        [ ("y", LocalVarReference "y")
                        ]
                      , scopeCellVariables = Map.fromList []
                      , scopeFreeVariables = Map.fromList
                        [ ("l", FreeVarReference "l")
                        ]
                      }
                    ]
                  , scopeLocalVariables = Map.fromList
                    [ 
                    ]
                  , scopeCellVariables = Map.fromList
                    [ ("l", CellVarReference "l")
                    ]
                  , scopeFreeVariables = Map.fromList
                    [ ("z", FreeVarReference "z")
                    ]
                  }
                ]
              , scopeLocalVariables = Map.fromList
                [ ("local", LocalVarReference "local")
                ]
              , scopeFreeVariables = Map.fromList
                [
                ]
              , scopeCellVariables = Map.fromList
                [ ("z", CellVarReference "z")
                ]
              }
            ]
        , scopeLocalVariables = Map.empty
        , scopeCellVariables = Map.empty
        , scopeFreeVariables = Map.empty
        }
      )

tests :: Test
tests = TestList
  [ TestLabel "nested lambdas test" testNestedLambdas ]
