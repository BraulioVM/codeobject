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
      (ResolvedProgram $ Scope 
        { scopeAST = FAtom (ConstantVarReference 0)
        , scopeConstants = []
        , scopeLocalVariables = Map.empty
        , scopeCellVariables = Map.empty
        , scopeFreeVariables = Map.empty
        }
      )

tests :: Test
tests = TestList
  [ TestLabel "nested lambdas test" testNestedLambdas ]
