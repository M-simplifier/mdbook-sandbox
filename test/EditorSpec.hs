module EditorSpec (spec) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Editor.Buffer
import Editor.Command
import Editor.Core
import Editor.Cursor

spec :: Spec
spec = do
  describe "invariants" $ do
    prop "applyCommand preserves cursor bounds" $
      forAll genEditor $ \editor ->
        forAll genCommand $ \cmd ->
          invariant (applyCommand cmd editor)

  describe "insertChar" $ do
    prop "in Insert mode, insertChar increases line length and column" $
      forAll genEditor $ \editor ->
        let editor' = editor { mode = Insert }
            Cursor r c = cursor editor'
            beforeLen = lineLength (buffer editor') r
            after = applyCommand (InsertChar 'x') editor'
            Cursor _ c' = cursor after
            afterLen = lineLength (buffer after) r
        in c' == c + 1 && afterLen == beforeLen + 1

  describe "backspace" $ do
    prop "in Insert mode, backspace does not increase line length" $
      forAll genEditor $ \editor ->
        let editor' = editor { mode = Insert }
            Cursor r _ = cursor editor'
            beforeLen = lineLength (buffer editor') r
            after = applyCommand Backspace editor'
            afterLen = lineLength (buffer after) r
        in afterLen <= beforeLen

  describe "examples" $ do
    it "moveRight from end of line goes to next line" $ do
      let buf = fromLines (T.pack "a" :| [T.pack "b"])
          editor = Editor buf (Cursor 0 1) Normal
          Editor _ (Cursor r c) _ = moveRight editor
      (r, c) `shouldBe` (1, 0)

invariant :: Editor -> Bool
invariant (Editor buf (Cursor r c) _) =
  let rows = numLines buf
  in rows > 0
     && r >= 0
     && r < rows
     && c >= 0
     && c <= lineLength buf r

genEditor :: Gen Editor
genEditor = do
  buf <- genBuffer
  cur <- genCursor buf
  mode' <- elements [Normal, Insert]
  pure (Editor buf cur mode')

genBuffer :: Gen Buffer
genBuffer = do
  lineCount <- chooseInt (1, 5)
  lines' <- vectorOf lineCount genLine
  pure (fromLines (NE.fromList lines'))

genLine :: Gen Text
genLine = T.pack <$> listOf (elements ['a' .. 'e'])

genCursor :: Buffer -> Gen Cursor
genCursor buf = do
  let rows = numLines buf
  row' <- chooseInt (0, rows - 1)
  let maxCol = lineLength buf row'
  col' <- chooseInt (0, maxCol)
  pure (Cursor row' col')

genCommand :: Gen Command
genCommand = oneof
  [ pure MoveLeft
  , pure MoveRight
  , pure MoveUp
  , pure MoveDown
  , InsertChar <$> elements ['a' .. 'c']
  , pure Backspace
  , pure EnterInsert
  , pure EnterNormal
  ]
