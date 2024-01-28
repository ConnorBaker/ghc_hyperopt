module Main where

import ParseBenchmarkTests qualified
import Test.Tasty ( defaultMain, testGroup, TestTree )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests for GHC-HyperOpt"
    [ ParseBenchmarkTests.tests
    ]