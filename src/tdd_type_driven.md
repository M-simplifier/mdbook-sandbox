# TDD and Type-Driven

## TDDのサイクル

1. 例示テストを書く
2. 最小実装で通す
3. 性質テストで抜けを探す
4. リファクタして整理する

この教材では、特に 2→3 の往復を重視します。

## 型駆動の活用

- `Buffer` を `NonEmpty` にすることで「空バッファ」を排除
- `Mode` を列挙型にして、モード分岐の漏れを防ぐ
- `Command` をADTにして、操作の集合を明示する

型は「仕様の圧縮」です。型の時点で不正な状態を除外できると、テストはより意味のある範囲に集中できます。

## 小さなTDDの実例: 改行挿入

1. 例示テストを置く

```haskell
it "insertNewline splits a line" $ do
  let editor = initialEditor (T.pack "ab")
      editor' = applyCommand EnterInsert editor
      Editor buf (Cursor r c) _ = applyCommand InsertNewline (editor' { cursor = Cursor 0 1 })
  toLines buf `shouldBe` (T.pack "a" :| [T.pack "b"])
  (r, c) `shouldBe` (1, 0)
```

2. 最小実装で通す
3. 性質テストで境界条件を広げる

```haskell
prop "insertNewline moves to next row" $
  forAll genEditor $ \editor ->
    let editor' = editor { mode = Insert }
        Cursor r _ = cursor editor'
        Cursor r' c' = cursor (applyCommand InsertNewline editor')
    in r' == r + 1 && c' == 0
```

この短いサイクルを繰り返すことで、仕様と実装がズレにくくなります。
