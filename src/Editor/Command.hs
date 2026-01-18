module Editor.Command
  ( Mode(..)
  , Command(..)
  ) where

data Mode
  = Normal
  | Insert
  deriving (Eq, Show)

data Command
  = MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | InsertChar Char
  | Backspace
  | EnterInsert
  | EnterNormal
  deriving (Eq, Show)
