module ResolvedProgramTests where

import qualified Data.Map as Map

import Test.HUnit

import Scheme.References.Internal
import Scheme.References
import Scheme.Types
import Scheme.AST

testNestedLambdas :: Test
testNestedLambdas = TestCase $ do
  assertEqual "resolved program" (resolveReferences program)
    (Right resolved)
  assertEqual "shrink scope" (shrinkScope resolved) shrink
  where
    program :: FAST
    program =
      FLambda ["x", "y"] (
      FBegin [ FDefine "z" (FAtom (AInt 5))
             , FDefine "local" (FAtom (AInt 6))
             , FLambda ["arg1", "arg2"] (
                 FBegin
                   [ FDefine "l" (FReference "z")
                   , FLambda ["arg3", "arg4"] (FDefine "y" (FReference "l"))
                   ]
                 )
             ]
      )

    resolved =
      (Scope 
        { scopeCode = FFuncRef 0 []
        , scopeConstants =
            [ Left $ Scope
              { scopeCode =
                  FBegin
                  [ FDefine "z"
                    (FAtom $ ConstantVarReference 0)
                  , FDefine "local"
                    (FAtom $ ConstantVarReference 1)
                  , FFuncRef 2 ["z"]
                  ]
              , scopeConstants =
                [ Right (AInt 5)
                , Right (AInt 6)
                , Left $ Scope
                  { scopeCode =
                      FBegin
                      [ FDefine "l"
                        (FReference "z")
                      , FFuncRef 0 ["l"]
                      ]
                  , scopeConstants =
                    [
                      Left $ Scope
                      { scopeCode =
                          FDefine "y"
                          (FReference "l")
                      , scopeConstants = []
                      , scopeVars = Map.fromList
                                    [ ("arg3", LocalScope)
                                    , ("arg4", LocalScope)
                                    , ("y", LocalScope)
                                    , ("l", FreeScope)
                                    ]
                      , scopeArgumentNames = ["arg3", "arg4"]
                      }
                    ]
                  , scopeVars = Map.fromList
                                [ ("arg1", LocalScope)
                                , ("arg2", LocalScope)
                                , ("l", CellScope)
                                , ("z", FreeScope)
                                ]
                  , scopeArgumentNames = ["arg1", "arg2"]
                  }
                ]
              , scopeVars = Map.fromList
                            [ ("x", LocalScope)
                            , ("y", LocalScope)
                            , ("local", LocalScope)
                            , ("z", CellScope)
                            ]
              , scopeArgumentNames = ["x", "y"]
              }
            ]
        , scopeVars = Map.empty
        , scopeArgumentNames = []
        }
      )

    shrink =
      (IndexedScope 
        { ixsCode = FAtom (ConstantVarReference 0)
        , ixsConstants =
            [ Left $ IndexedScope
              { ixsCode =
                  FBegin
                  [ FDefine (CellVarReference 0)
                    (FAtom $ ConstantVarReference 0)
                  , FDefine (LocalVarReference 0)
                    (FAtom $ ConstantVarReference 1)
                  , FAtom (ConstantVarReference 2)
                  ]
              , ixsConstants =
                [ Right (AInt 5)
                , Right (AInt 6)
                , Left $ IndexedScope
                  { ixsCode =
                      FBegin
                      [ FDefine (CellVarReference 0)
                        (FReference (FreeVarReference 0))
                      , FAtom (ConstantVarReference 0)
                      ]
                  , ixsConstants =
                    [
                      Left $ IndexedScope
                      { ixsCode =
                          FDefine (LocalVarReference 2)
                          (FReference (FreeVarReference 0))
                      , ixsConstants = []
                      , ixsFree = ["l"]
                      , ixsLocal = ["arg3", "arg4", "y"]
                      , ixsCell = []
                      , ixsArgumentNames = ["arg3", "arg4"]
                      }
                    ]
                  , ixsFree = ["z"]
                  , ixsCell = ["l"]
                  , ixsLocal = ["arg1", "arg2"]
                  , ixsArgumentNames = ["arg1", "arg2"]
                  }
                ]
              , ixsFree = []
              , ixsCell = ["z"]
              , ixsLocal = ["local", "x", "y"]
              , ixsArgumentNames = ["x", "y"]
              }
            ]
        , ixsFree = []
        , ixsLocal = []
        , ixsCell = []
        , ixsArgumentNames = []
        }
      )

tests :: Test
tests = TestList
  [ TestLabel "nested lambdas test" testNestedLambdas ]
