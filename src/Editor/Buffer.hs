module Editor.Buffer
  ( Buffer
  , fromLines
  , toLines
  , numLines
  , lineAt
  , lineLength
  , setLine
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

newtype Buffer = Buffer (NonEmpty Text)
  deriving (Eq, Show)

fromLines :: NonEmpty Text -> Buffer
fromLines = Buffer

toLines :: Buffer -> NonEmpty Text
toLines (Buffer ls) = ls

numLines :: Buffer -> Int
numLines (Buffer ls) = NE.length ls

lineAt :: Int -> Buffer -> Text
lineAt idx (Buffer ls) = NE.toList ls !! idx

lineLength :: Buffer -> Int -> Int
lineLength buf idx = T.length (lineAt idx buf)

setLine :: Int -> Text -> Buffer -> Buffer
setLine idx newLine (Buffer ls) = Buffer (replaceAt idx newLine ls)

replaceAt :: Int -> a -> NonEmpty a -> NonEmpty a
replaceAt idx newVal ls =
  let (prefix, rest) = splitAt idx (NE.toList ls)
  in case rest of
       [] -> ls
       (_:xs) -> case prefix ++ (newVal : xs) of
                   (y:ys) -> y :| ys
                   [] -> ls
