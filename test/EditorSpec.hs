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
            editorAfter = applyCommand (InsertChar 'x') editor'
            Cursor _ c' = cursor editorAfter
            afterLen = lineLength (buffer editorAfter) r
        in c' == c + 1 && afterLen == beforeLen + 1

  describe "backspace" $ do
    prop "in Insert mode, backspace does not increase line length" $
      forAll genEditor $ \editor ->
        let editor' = editor { mode = Insert }
            Cursor r _ = cursor editor'
            beforeLen = lineLength (buffer editor') r
            editorAfter = applyCommand Backspace editor'
            afterLen = lineLength (buffer editorAfter) r
        in afterLen <= beforeLen

  describe "insertNewline" $ do
    prop "in Insert mode, insertNewline splits the line and moves cursor" $
      forAll genEditor $ \editor ->
        let editor' = editor { mode = Insert }
            Cursor r c = cursor editor'
            beforeLine = lineAt r (buffer editor')
            editorAfter = applyCommand InsertNewline editor'
            Cursor r' c' = cursor editorAfter
            beforePrefixLen = T.length (T.take c beforeLine)
            beforeSuffixLen = T.length (T.drop c beforeLine)
            afterPrefixLen = lineLength (buffer editorAfter) r
            afterSuffixLen = lineLength (buffer editorAfter) (r + 1)
        in r' == r + 1
           && c' == 0
           && afterPrefixLen == beforePrefixLen
           && afterSuffixLen == beforeSuffixLen

  describe "deleteLine" $ do
    prop "in Normal mode, deleteLine reduces line count unless single line" $
      forAll genEditor $ \editor ->
        let editor' = editor { mode = Normal }
            beforeLines = numLines (buffer editor')
            editorAfter = applyCommand DeleteLine editor'
            afterLines = numLines (buffer editorAfter)
        in if beforeLines > 1 then afterLines == beforeLines - 1 else afterLines == 1

  describe "examples" $ do
    it "moveRight from end of line goes to next line" $ do
      let buf = fromLines (T.pack "a" :| [T.pack "b"])
          editor = Editor buf (Cursor 0 1) Normal
          Editor _ (Cursor r c) _ = moveRight editor
      (r, c) `shouldBe` (1, 0)

  describe "model-based" $ do
    prop "model matches editor for random command sequences" $
      forAll genEditor $ \editor ->
        forAll genCommandSeq $ \cmds ->
          let model0 = modelFromEditor editor
              editor' = applyCommands cmds editor
              model' = applyCommandsModel cmds model0
          in modelMatches editor' model'

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

genCommandSeq :: Gen [Command]
genCommandSeq = sized $ \n -> do
  len <- chooseInt (0, min 30 (n + 5))
  vectorOf len genCommand

applyCommands :: [Command] -> Editor -> Editor
applyCommands cmds editor = foldl (flip applyCommand) editor cmds

data Model = Model
  { modelLines :: NonEmpty Text
  , modelCursor :: Cursor
  , modelMode :: Mode
  } deriving (Eq, Show)

modelFromEditor :: Editor -> Model
modelFromEditor (Editor buf cur mode') = Model (toLines buf) cur mode'

modelMatches :: Editor -> Model -> Bool
modelMatches (Editor buf cur mode') (Model ls cur' mode'') =
  toLines buf == ls && cur == cur' && mode' == mode''

applyCommandsModel :: [Command] -> Model -> Model
applyCommandsModel cmds model = foldl (flip applyCommandModel) model cmds

applyCommandModel :: Command -> Model -> Model
applyCommandModel cmd model@(Model ls cur mode') =
  case cmd of
    MoveLeft -> moveLeftModel model
    MoveRight -> moveRightModel model
    MoveUp -> moveUpModel model
    MoveDown -> moveDownModel model
    InsertChar ch ->
      if mode' == Insert then insertCharModel ch model else model
    InsertNewline ->
      if mode' == Insert then insertNewlineModel model else model
    Backspace ->
      if mode' == Insert then backspaceModel model else model
    DeleteLine ->
      if mode' == Normal then deleteLineModel model else model
    EnterInsert -> Model ls cur Insert
    EnterNormal -> Model ls cur Normal

moveLeftModel :: Model -> Model
moveLeftModel (Model ls (Cursor r c) mode') =
  let rows = NE.length ls
  in if c > 0
       then Model ls (Cursor r (c - 1)) mode'
       else if r > 0 && rows > 0
              then let prevLen = lineLengthModel (r - 1) ls
                   in Model ls (Cursor (r - 1) prevLen) mode'
              else Model ls (Cursor r c) mode'

moveRightModel :: Model -> Model
moveRightModel (Model ls (Cursor r c) mode') =
  let rows = NE.length ls
      curLen = lineLengthModel r ls
  in if c < curLen
       then Model ls (Cursor r (c + 1)) mode'
       else if r + 1 < rows
              then Model ls (Cursor (r + 1) 0) mode'
              else Model ls (Cursor r c) mode'

moveUpModel :: Model -> Model
moveUpModel (Model ls (Cursor r c) mode') =
  if r > 0
    then let newRow = r - 1
             newCol = min c (lineLengthModel newRow ls)
         in Model ls (Cursor newRow newCol) mode'
    else Model ls (Cursor r c) mode'

moveDownModel :: Model -> Model
moveDownModel (Model ls (Cursor r c) mode') =
  let rows = NE.length ls
  in if r + 1 < rows
       then let newRow = r + 1
                newCol = min c (lineLengthModel newRow ls)
            in Model ls (Cursor newRow newCol) mode'
       else Model ls (Cursor r c) mode'

insertCharModel :: Char -> Model -> Model
insertCharModel ch (Model ls (Cursor r c) mode') =
  let line = lineAtModel r ls
      (prefix, suffix) = T.splitAt c line
      newLine = prefix <> T.singleton ch <> suffix
      newLines = setLineModel r newLine ls
  in Model newLines (Cursor r (c + 1)) mode'

insertNewlineModel :: Model -> Model
insertNewlineModel (Model ls (Cursor r c) mode') =
  let line = lineAtModel r ls
      (prefix, suffix) = T.splitAt c line
      lines' = setLineModel r prefix ls
      lines'' = insertLineAfterModel r suffix lines'
  in Model lines'' (Cursor (r + 1) 0) mode'

backspaceModel :: Model -> Model
backspaceModel (Model ls (Cursor r c) mode')
  | c <= 0 = Model ls (Cursor r c) mode'
  | otherwise =
      let line = lineAtModel r ls
          beforeText = T.take (c - 1) line
          afterText = T.drop c line
          newLine = beforeText <> afterText
          newLines = setLineModel r newLine ls
      in Model newLines (Cursor r (c - 1)) mode'

deleteLineModel :: Model -> Model
deleteLineModel (Model ls (Cursor r c) mode') =
  let xs = NE.toList ls
      lines' = case xs of
                 [_] -> T.empty :| []
                 _ ->
                   let (prefix, rest) = splitAt r xs
                   in case rest of
                        [] -> ls
                        (_:ys) -> case prefix ++ ys of
                                    (y:ys') -> y :| ys'
                                    [] -> T.empty :| []
      rows = NE.length lines'
      newRow = min r (rows - 1)
      newCol = min c (lineLengthModel newRow lines')
  in Model lines' (Cursor newRow newCol) mode'

lineAtModel :: Int -> NonEmpty Text -> Text
lineAtModel idx ls = NE.toList ls !! idx

lineLengthModel :: Int -> NonEmpty Text -> Int
lineLengthModel idx ls = T.length (lineAtModel idx ls)

setLineModel :: Int -> Text -> NonEmpty Text -> NonEmpty Text
setLineModel idx newLine ls =
  let (prefix, rest) = splitAt idx (NE.toList ls)
  in case rest of
       [] -> ls
       (_:xs) -> case prefix ++ (newLine : xs) of
                   (y:ys) -> y :| ys
                   [] -> ls

insertLineAfterModel :: Int -> Text -> NonEmpty Text -> NonEmpty Text
insertLineAfterModel idx newLine ls =
  let (prefix, rest) = splitAt (idx + 1) (NE.toList ls)
  in case prefix ++ (newLine : rest) of
       (y:ys) -> y :| ys
       [] -> ls
