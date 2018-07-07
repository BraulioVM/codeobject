module TestPyMonad where

import Test.HUnit

import Util

import PyMonad
import Types

testPyMonad :: Test
testPyMonad = testCodeObjectOutput co ["30", "'hey'", "'hey'", "30"]
  where
    co = createCodeObject $ do
      x <- makeComp <$> (defConstant $ PyInt 30)
      y <- makeComp <$> (defConstant $ PyString "hey")
    
      evX <- evaluate x
      evY <- evaluate y

      printValue evX
      printValue evY
      printValue evY
      printValue evX

testPyMonadSum :: Test
testPyMonadSum = testCodeObjectOutput co ["90", "60", "120", "90"]
  where
    co = createCodeObject $ do
      x <- makeComp <$> (defConstant $ PyInt 30)
      y <- makeComp <$> (defConstant $ PyInt 60)

      printComp (sumop x y)
      printComp (sumop x x)
      printComp (sumop y y)
      printComp (sumop y x)


tests :: Test
tests = TestList
  [ TestLabel "basic pymonad test" testPyMonad
  , TestLabel "pymonad sumop" testPyMonadSum
  ]
