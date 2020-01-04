# "Compiling with Continuations" 読書ノート

[Compiling with Continuations (English Edition)](https://www.amazon.co.jp/Compiling-Continuations-English-Andrew-Appel-ebook/dp/B00E3UR010)

## 紹介文

- プログラムのデータフローは継続で表現可能
- 継続のコンセプトは表示的意味論に由来していて、現実のコンパイラに応用可能
- 継続渡しスタイルの中間言語としての使い方と、それに最適化とプログラム変換をかける方法を詳解
- 継続はたいていの言語のコンパイルに使えるが、ここでは SML で例示
- 都度、説明がなされるので、ML の事前知識は不要
- プログラミング言語の理論に由来するコンセプトを SML のようなモダンな言語の最適化コンパイラに適用する方法を示している最初の本
- 産業界・学術界のコンパイラ作者にも、プログラミング言語の理論を学ぶ学生にもおすすめ

## 1. 概要

- ML はさまざまな言語機能を持つモダンな言語
- SML for New Jersey は ML 自身で書かれている最適化コンパイラおよびランタイム
- 中間言語として継続渡しスタイルを使用
- ML に詳しくない読者は Appendix A を参照

### 1.1 継続渡しスタイル (CPS)

- FORTRAN の利点はアセンブリより高級で中間結果の名前付けが不要
- ラムダ計算にも同様の利点、そして関数にさえ名前が不要、評価順序が未規定、自由で書きやすい
- CPS はすべての制御フローとデータフローを明示的にする記法
- CPS の概要、関数呼び出しが暗黙的に受け取る return アドレスを継続として引数で受け取るのが CPS

### 1.2 CPS の利点

- 他の表現と比較して CPS の利点を列挙
- ラムダ計算
- QUAD
- PDG
- SSA

インライン展開

- よくあるβ簡約をやると評価順序や回数が変わるので正格評価で非純粋な言語だと問題
- CPS/QUAD/PDG/SSA ならOK

クロージャ表現

- 関数が外側の関数のローカル変数を参照しているような状態の扱い
- CPS/ラムダ計算だと容易
- QUAD/PDG/SSA は1個の関数のデータフローやコントロールフローに関する表現なのでこの問題に関してどうとは言い難い

データフロー解析

- CPS/SSA だとやりやすい
- ラムダ計算だと厳しい

レジスタ割当

略

ベクトル化

略

スケジューリング

略

結論

- 継続によりクリーンな代入演算とノイマンマシンのためのデータフロー・レジスタ解析が手に入る

(これ結論か？)

### 1.3 ML とは何か

- ML の起源
- ML の利点
    - 正格
    - 高階
    - パラメトリック多相
    - 静的型付け
    - GC
    - variable bindings are statically determined (レキシカルスコープ)
    - 副作用 (非純粋性)
    - 形式化された意味論
- クロージャのライフタイムが曖昧なのでGCが必須
- マクロがない
- データ構造が immutable なのは特徴的 (Haskell系もでは？)
    - 別名問題 (aliasing problem) が発生しない

### 1.4 Compiler organization

- 字句解析、構文解析、型検査、アノテーション付き抽象構文木
    - フロントエンド
    - この本では省略
- 単純なラムダ計算風の表現への変換 (4章)
- CPS への変換 (5章)
- 最適化をかけてより良い CPS 表現に変換 (6-9章)
- クロージャ変換により自由変数をなくす (10章)
- 入れ子のスコープの消去 (10章)
- レジスタのスピル (11章)
- アセンブリへの変換 (13章)
- 命令スケジューリング、ジャンプサイズ最適化、バックパッチング、コード生成 (14章)

## 2. 継続渡しスタイル (CPS)

- CPS の起源 (Scheme)
- 本章は CPS の構造と挙動について
- 重要な点はプリミティブの引数が原子式に限られること

### 2.1 CPS データ型

```fs
// 本では SML だけど、ここでは F# で書く。
// F# に sig はないので structure に置き換えている。
// (ジェネリクスや制約などを使えば sig と同等のコードをかけるけど、かなり冗長になる。)

// some T : equality
type Var =
    | Var
        of string

type Value =
    | VarValue
        of Var

    | LabelValue
        of Var

    | IntValue
        of int

    | RealValue
        of string

    | StringValue
        of string

type AccessPath =
    | OffsetPath
        of int

    | SelectPath
        of int * AccessPath

type PrimOp =
    // +
    | Add
    // -
    | Sub
    // *
    | Mul
    | Div
    // ~
    | Mod

    | IEq
    | INeq
    // <
    | Lt
    // <=
    | LtEq
    // >
    | Gt
    // >=
    | GtEq

    | RangeChk
    // !
    | Deref
    | Subscript
    | OrderOf
    // それとたくさん！

type CExp =
    | CRecordExp
        of (Value * AccessPath) list * Var * CExp

    | CSelectExp
        of int * Value * Var * CExp

    | COffsetExp
        of int * Value * Var * CExp

    | CAppExp
        of Value * Value list

    | CFixExp
        of (Var * Var list * CExp) list * CExp

    | CSwitchExp
        of Value * CExp list

    | CPrimOpExp
        of PrimOp * args:Value list * result:Var list * ks:CExp list
```
