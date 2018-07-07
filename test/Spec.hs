{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.HUnit

import Util (withDirectory, testDirectory)
import qualified BasicTests (tests)
import qualified TestPyMonad (tests)

tests :: Test
tests = TestList
  [ TestLabel "Basic tests" BasicTests.tests
  , TestLabel "PyMonad tests" TestPyMonad.tests
  ]

main :: IO ()
main = do
  withDirectory testDirectory (const () <$> runTestTT tests)
