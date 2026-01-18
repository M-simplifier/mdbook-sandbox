module Editor.Core
  ( Editor(..)
  , initialEditor
  , applyCommand
  , moveLeft
  , moveRight
  , moveUp
  , moveDown
  , insertChar
  , backspace
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as T

import Editor.Buffer
import Editor.Command
import Editor.Cursor

data Editor = Editor
  { buffer :: Buffer
  , cursor :: Cursor
  , mode :: Mode
  } deriving (Eq, Show)

initialEditor :: Text -> Editor
initialEditor line =
  Editor (fromLines (line :| [])) (Cursor 0 0) Normal

applyCommand :: Command -> Editor -> Editor
applyCommand cmd editor@(Editor buf cur mode) =
  case cmd of
    MoveLeft -> moveLeft editor
    MoveRight -> moveRight editor
    MoveUp -> moveUp editor
    MoveDown -> moveDown editor
    InsertChar ch ->
      if mode == Insert then insertChar ch editor else editor
    Backspace ->
      if mode == Insert then backspace editor else editor
    EnterInsert -> Editor buf cur Insert
    EnterNormal -> Editor buf cur Normal

moveLeft :: Editor -> Editor
moveLeft (Editor buf (Cursor r c) mode)
  | c > 0 = Editor buf (Cursor r (c - 1)) mode
  | r > 0 =
      let prevLen = lineLength buf (r - 1)
      in Editor buf (Cursor (r - 1) prevLen) mode
  | otherwise = Editor buf (Cursor r c) mode

moveRight :: Editor -> Editor
moveRight (Editor buf (Cursor r c) mode)
  | c < lineLength buf r = Editor buf (Cursor r (c + 1)) mode
  | r + 1 < numLines buf = Editor buf (Cursor (r + 1) 0) mode
  | otherwise = Editor buf (Cursor r c) mode

moveUp :: Editor -> Editor
moveUp (Editor buf (Cursor r c) mode)
  | r > 0 =
      let newRow = r - 1
          newCol = min c (lineLength buf newRow)
      in Editor buf (Cursor newRow newCol) mode
  | otherwise = Editor buf (Cursor r c) mode

moveDown :: Editor -> Editor
moveDown (Editor buf (Cursor r c) mode)
  | r + 1 < numLines buf =
      let newRow = r + 1
          newCol = min c (lineLength buf newRow)
      in Editor buf (Cursor newRow newCol) mode
  | otherwise = Editor buf (Cursor r c) mode

insertChar :: Char -> Editor -> Editor
insertChar ch (Editor buf (Cursor r c) mode) =
  let line = lineAt r buf
      (prefix, suffix) = T.splitAt c line
      newLine = prefix <> T.singleton ch <> suffix
  in Editor (setLine r newLine buf) (Cursor r (c + 1)) mode

backspace :: Editor -> Editor
backspace (Editor buf (Cursor r c) mode)
  | c <= 0 = Editor buf (Cursor r c) mode
  | otherwise =
      let line = lineAt r buf
          before = T.take (c - 1) line
          after = T.drop c line
          newLine = before <> after
      in Editor (setLine r newLine buf) (Cursor r (c - 1)) mode
