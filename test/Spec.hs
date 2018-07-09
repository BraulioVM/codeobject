{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.HUnit

import Util (withDirectory, testDirectory)
import qualified BasicTests (tests)
import qualified TestPyMonad (tests)
import qualified SchemeParserTests (tests)
import qualified ResolvedProgramTests (tests)

tests :: Test
tests = TestList
  [ TestLabel "Basic tests" BasicTests.tests
  , TestLabel "PyMonad tests" TestPyMonad.tests
  , TestLabel "Scheme parser tests" SchemeParserTests.tests
  , TestLabel "Resolved program tests" ResolvedProgramTests.tests
  ]

main :: IO ()
main = do
  withDirectory testDirectory (const () <$> runTestTT tests)
