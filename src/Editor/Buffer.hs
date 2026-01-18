module Editor.Buffer
  ( Buffer
  , fromLines
  , toLines
  , numLines
  , lineAt
  , lineLength
  , setLine
  , insertLineAfter
  , deleteLine
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

insertLineAfter :: Int -> Text -> Buffer -> Buffer
insertLineAfter idx newLine (Buffer ls) =
  let (prefix, rest) = splitAt (idx + 1) (NE.toList ls)
  in case prefix ++ (newLine : rest) of
       (y:ys) -> Buffer (y :| ys)
       [] -> Buffer ls

deleteLine :: Int -> Buffer -> Buffer
deleteLine idx (Buffer ls) =
  let xs = NE.toList ls
  in case xs of
       [_] -> Buffer (T.empty :| [])
       _ ->
         let (prefix, rest) = splitAt idx xs
         in case rest of
              [] -> Buffer ls
              (_:ys) -> case prefix ++ ys of
                          (y:ys') -> Buffer (y :| ys')
                          [] -> Buffer (T.empty :| [])

replaceAt :: Int -> a -> NonEmpty a -> NonEmpty a
replaceAt idx newVal ls =
  let (prefix, rest) = splitAt idx (NE.toList ls)
  in case rest of
       [] -> ls
       (_:xs) -> case prefix ++ (newVal : xs) of
                   (y:ys) -> y :| ys
                   [] -> ls
