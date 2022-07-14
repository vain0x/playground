# Practical Compiler Construction

「実践コンパイラ構成法」を読む

## 実装したもの

本の解説・実装に基づいて、simple言語の処理系を F# で実装した

- [x] lexer
- [ ] ~~yacc~~ ([grammar_issue.md](grammar_issue.md)を参照)
- [x] 手書きの再帰下降パーサ
- 言語機能
    - [x] int型のローカル変数
    - [ ] int型の配列
    - [x] if, while
    - [x] ローカル関数の呼び出し (静的リンク方式。関数オブジェクトは不可)
- プリミティブ
    - [ ] 数値の算術演算、比較
    - [ ] 数値の入力 (scan)
    - [x] 数値の出力
    - [x] (固定の)文字列の出力
- [x] ネイティブコンパイル: Linux-x64用のアセンブリを出力する
