# Editor Core

この章ではエディタの最小実装を構築します。実装は小さく、テストが追える範囲に留めます。

## コアAPI

以下の関数を中心に実装します。

- `applyCommand :: Command -> Editor -> Editor`
- `insertChar :: Char -> Editor -> Editor`
- `insertNewline :: Editor -> Editor`
- `backspace :: Editor -> Editor`
- `moveLeft` / `moveRight` / `moveUp` / `moveDown`
- `deleteLine :: Editor -> Editor`

## 仕様の優先順位

1. 不変条件を守る
2. 直感的なカーソル挙動 (端に到達したら止まる、または次行へ)
3. モードによる差を後から足す

最初は「文字の挿入・削除とカーソル移動が壊れないこと」を最優先にします。

## 例示テストの導入

最初の例示テストは、仕様を固定するために使います。

- 空行で挿入すると行の長さが1増える
- 行頭での`backspace`は行を変えない
- 行末から`moveRight`は次行の先頭に移る
- `insertNewline`は行を分割し、カーソルを次行先頭に移す
- `deleteLine`は現在行を削除し、バッファが空にならないように保つ

それ以外はPBTに任せる方針で進めます。
