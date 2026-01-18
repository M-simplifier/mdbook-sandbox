module Main where

import qualified Data.Text as T

import Editor.Command
import Editor.Core

main :: IO ()
main = do
  let editor = initialEditor (T.pack "")
      editor' = applyCommand EnterInsert editor
      editor'' = applyCommand (InsertChar 'h') editor'
  print editor''
