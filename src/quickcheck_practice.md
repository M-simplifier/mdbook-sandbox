# QuickCheck in Practice

## 生成器 (Generator) を設計する

QuickCheckの効果は生成器で決まります。エディタの場合、ランダムな`Editor`を作るには不変条件を満たした状態を必ず生成する必要があります。

設計方針:

- まず`Buffer`を生成する
- その`Buffer`の範囲内で`Cursor`を生成する
- モードは独立に選ぶ

## 代表的な性質

- `applyCommand` 後も不変条件が保たれる
- `insertChar` の後は必ずカーソルが右へ動く
- `backspace` は行長を増やさない
- Normal/Insertで無効なコマンドは状態を変えない

このように「必ず成り立つはずの性質」を列挙し、最初の防波堤にします。

## Shrinkの視点

失敗時に「最小ケース」を得るため、`Buffer`や`Cursor`のShrinkも意識します。特に行数と列数が減るように設計すると、デバッグが圧倒的に楽になります。

### Shrinkが効かないときの例

例えば「カーソルが範囲外になる」失敗が出たとしても、巨大なバッファと長いコマンド列のままだと原因が見えません。

```
*** Failed! Falsified (after 31 tests):
buffer = 5 lines, cursor = (3, 18)
commands = [MoveRight, MoveRight, InsertNewline, ...]
```

ここから「1行・短いコマンド列」に縮められるように、Shrinkで行数と列数を優先的に減らすのがポイントです。Shrinkの設計は、PBTのデバッグ効率を左右します。
