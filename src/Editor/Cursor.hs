module Editor.Cursor
  ( Cursor(..)
  ) where

data Cursor = Cursor
  { row :: Int
  , col :: Int
  } deriving (Eq, Show)
