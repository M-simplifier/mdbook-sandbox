module Main where

import Test.Hspec
import qualified EditorSpec

main :: IO ()
main = hspec EditorSpec.spec
