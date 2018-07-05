module TestPyMonad where

import Test.HUnit

import Util

import PyMonad
import Types

testPyMonad :: Test
testPyMonad = testCodeObjectOutput co ["30", "'hey'", "'hey'", "30"]
  where
    co = createCodeObject $ do
      x <- defConstant $ PyInt 30
      y <- defConstant $ PyString "hey"
      printRef x
      printRef y
      printRef y
      printRef x


tests :: Test
tests = TestList
  [ TestLabel "basic pymonad test" testPyMonad
  ]
