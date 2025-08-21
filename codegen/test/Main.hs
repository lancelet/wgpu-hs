module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import YAML.TypesTest qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "All Tests"
      [ YAML.TypesTest.tests
      ]