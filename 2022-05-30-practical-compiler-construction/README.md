# Practical Compiler Construction

「実践コンパイラ構成法」を読む

## 実装したもの

本の解説・実装に基づいて、simple言語の処理系を F# で実装した

- [x] lexer
    - [x] 数値、文字列、識別子
    - [x] コメント
- [ ] ~~yacc~~ ([grammar_issue.md](grammar_issue.md)を参照)
- [x] 手書きの再帰下降パーサ
- 言語機能
    - [x] int型のローカル変数
    - [x] int型の固定長配列
    - [x] if, while
    - [x] ローカル関数の呼び出し (静的リンク方式。関数オブジェクトは不可)
    - [x] type宣言
- プリミティブ
    - [x] 数値の算術演算、比較
    - [x] 数値の入力 (scan)
    - [x] 数値の出力
    - [x] (固定の)文字列の出力
- [x] ネイティブコンパイル: Linux-x64用のアセンブリを出力する

### その他

- [ ] 文字列のエスケープシーケンスを適切に実装する
- [ ] returnを型検査する
- [ ] 配列型の等価性の判定を修正する ([tests/type_array_synonym.simple](tests/type_array_synonym.simple)を修正)
- [ ] ループのブロックの先頭でローカル変数を宣言するとスタックオーバーフローする問題を修正する

軽微な追加機能

- [ ] break, continue
- [ ] for
- [ ] `+=` operator
- [ ] `&&` operator

実用的な機能

- [ ] 動的な文字列、int以外の配列、動的な配列など

----

- [x] [UseFsYacc](UseFsYacc): FxLex/FsYaccを使ってレキサーとパーサーを生成する
