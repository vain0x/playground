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
- 重要な点はプリミティブの引数が原子式 (変数か定数) に限られること

### 2.1 CPS データ型

```fs
// 本では SML だけど、ここでは F# で書く。
// F# に sig はないので structure に置き換えている。
// (ジェネリクスや制約などを使えば sig と同等のコードをかけるけど、かなり冗長になる。)

/// 変数を表す何らかの識別子
type Var =
    | Var
        of string

// 変数項または定数項
type Value =
    | VarValue
        of Var

    /// クロージャ変換後において「関数」を指す定数。2.4 参照
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
    /// レコードの構築
    | CRecordExp
        of (Value * AccessPath) list * Var * CExp

    /// レコードからフィールドの値を取り出して結果を束縛
    | CSelectExp
        of int * Value * Var * CExp

    /// 変数がレコードの i 番目のフィールドを指すとき、
    /// i+j 番目のフィールドを指す値を変数に束縛
    /// (カーソルを動かす感じ？)
    | COffsetExp
        of int * Value * Var * CExp

    | CAppExp
        of Value * Value list

    /// 相互に再帰可能な関数の定義
    | CFixExp
        of (Var * Var list * CExp) list * CExp

    /// i 番目の継続にジャンプ
    | CSwitchExp
        of Value * CExp list

    | CPrimOpExp
        of PrimOp * args:Value list * result:Var list * ks:CExp list
```

以下の CPS 式は変数 a, b の値を足したものを変数 c に束縛して、その後、式 E を評価する。

```
    PRIM(+, [VAR a, VAR b], [c], [E])
```

導入される変数や継続は1つとは限らない。以下の CPS 式は `a < b` なら継続 F を実行し、そうでなければ G を実行する。

```
    PRIM(<, [VAR a, VAR b], [], [F, G])
```

### 2.2 エスケープする関数

- 外部に公開される関数は **エスケープする** という
- エスケープする関数はどこでどのように使われるのか分からない
- エスケープする関数に制約をかける
    - 1引数または2引数である
    - 2引数(ユーザー関数)なら、2個目の引数はエスケープする1引数関数(継続)である
    - 例外ハンドラは常に、エスケープする1引数関数(継続)である

### 2.3 スコープルール

- レキシカルスコープの説明
- CPS 式が導入した変数は、その式の継続の中で参照できる
- FIX が導入した関数は他の FIX の継続でも参照できる
- それ以外では参照できない

### 2.4 クロージャ変換

- CPS はノイマン型マシンの命令セットに近い (+ とか)
- 関数が自由変数を持てる点は異なる
- ノイマン型マシンの関数はただのアドレス
- 関数の表現としてクロージャを使うことが多い
- クロージャ = アドレスと自由変数の情報のペア
- クロージャ変換とは自由変数のない CPS に変換すること
- クロージャレコード = 自由変数の情報が入ったオブジェクト
- クロージャの呼び出し (f, x)(args...) → f(x, args...)
    - ここで x はクロージャレコードへのポインタか何か
    - クロージャレコードの詳細は f が知っているので呼び出し側は知らなくていい
    - (C言語的にいえば x はクロージャレコードである構造体への void* でいい)
- クロージャ変換後の関数は自由変数を持たないので定数として扱える
    - ラベルと呼ぶ
    - VAR と違って、レジスタを割り当てなくていいことが明確になる
- 自由変数ルール
    - 関数の本体に含まれる変数は、仮引数に含まれる変数か、同時に定義されている関数のみ
- FIX の中に FIX を置く意味がないのですべての関数を1個の FIX で定義する形式に変換していい
- 詳細は10章

### 2.5 スピル

- CPS の変数はノイマン型マシンのレジスタに似ている
- CPS の変数は無限にある、レジスタは有限にしかない
- 同時に生存していない変数は単一のレジスタに割り当てられる
- 生存変数は CPS の自由変数と同じ
    - (らしい。典型的なデータフロー解析の生存解析を知らないのでよく分からない)
- 詳細は11章

## 3. CPS の意味論

CPS の表示的意味論について。

略

## 4. ML 固有の最適化

- CPS 中間表現にする前の段階でした方がいい解析・変換・最適化もある

### 4.1 データ表現

- ML にはレコード型 (イミュータブルな直積) と datatype (直和) がある
- レコード型の値は名前を 0, 1, 2, ... に置き換えてタプルで表現する
- タグが1個の datatype は「透過的に」(transparently)レコードで表現できる
- 1個の値だけ持ち運ぶ(フィールドが1個の)コンストラクタの値は2ワードで表現できる
- ポインタと整数がランタイムにおいて識別可能なら可能な最適化がある
- 以下略

### 4.2 パターンマッチ

- パターンマッチのための比較の順序を選択することは ML 系言語のコンパイラの重要かつ非自明な仕事の1つ
- パターンと式のペアをルールと呼ぶ。マッチは引数とルールの列からなる
- ルールを前から順番に1つずつ試していく素朴な方法もある
- 決定木を使う
- 内部ノードはテストに対応し、子要素への枝にはそのテスト結果のラベルがついていて、枝の先のノードは残りのケースの候補になる
- 決定木からコードの生成は容易
- 網羅性と冗長性
- テストノードの個数が最小な決定木を作りたい
- 厳密な最適化はNP完全なのでヒューリスティックを使う
- datatype のマッチではタグごとに分類してからそれぞれの枝で値をパターンマッチにかける
- record のマッチでは各要素を1つ1つパターンマッチにかける

### 4.3 同値性

- datatype/record は構造的同値性で比較される
- ref や関数はアドレスの同値性で比較される
- 閉路は必ず ref を含むので比較が必ず停止する
- 比較される型ごとに比較関数を生成すると効率的
- 比較される値の型が多相だとランタイムタグを使って比較することになる
- 略
- 多相的な同値性は面白くない (no fun at all)

### 4.4 unboxed update

- ref や配列要素を更新するときに中身が boxed か unboxed かで使う命令を分けている
- CPS に変換して型情報が消えてしまう前に何らかのマークをつけておく方がいい

### 4.5 ミニ ML 部分言語

「ミニ ML」(mini-ML)

- ML をミニ ML に変換してから上記の操作をすると楽
- データ型
    - 整数、実数、文字列
    - datatype
    - タプル
    - 可変な配列
    - 1引数1結果の関数
- レコードはタプルにする
- 式
    - 整数、実数、文字列のリテラル
    - データコンストラクタの適用
    - データコンストラクタの除去
    - とてもシンプルなケース式
        - 定数パターン
        - コンストラクタパターン `K _`
        - ワイルドカードパターン `_` (最後のルールのみ)
        - 変数が束縛されない
    - タプルの構築
    - タプルの射影
    - 正格な関数適用
    - ラムダ抽象による関数定義
    - let val rec による相互再帰関数定義
    - 算術・比較演算
    - 参照や配列のための操作
    - 単純な例外処理
- パターンマッチや抽象型、モジュールシステムはない
- datatype はパターンマッチで分解するのではなく射影で分解する

### 4.6 例外宣言 (exception declarations)

- exception は val になる
    - `exception E of int` → `val E = ref "E"`
- `string` と違って `string ref` の比較はアドレス比較なので高速

### 4.7 ラムダ言語

- ミニML → ラムダ言語 → CPS
- 略

### 4.8 モジュールシステム

- モジュールはレコードに変換できる
    - type は消える
    - val, fun だけ残る
- 略

## 5. CPS への変換

略

## 6. CPS の最適化

- β簡約や定数畳み込みなどにより小さくて効率的な CPS に変換する
- FIX の表現がノイマン型マシンにそぐわない (自由変数を持つ) ので変形する
- 最終的にマシンコードにする
- 変換をプログラムがほとんど改善されなくなるまで繰り返す
- 厳密な最適化はできないのでヒューリスティックを多用する
- 計測してどのくらい最適化できたか調べる

### 6.1 定数畳み込みと β縮約(beta contraction)

- 算術演算の除去、既知のレコードからの射影の除去など

β縮約

- FIX 宣言において変数 f が関数として静的に束縛されて、f が引数に適用されるなら、β簡約が行える
- CPS 式が一意束縛性 (ある変数が一箇所でしか束縛されない) を持つなら、ラムダ式にある「変数捕獲」の問題はない
- β簡約の後、生成されたコードをα変換して一意束縛性を取り戻す
- 1回しか呼ばれない関数をβ簡約した後はα変換するより定義を削除する方がシンプル
- 1回だけ呼ばれる関数のβ簡約をβ縮約と呼ぶことにする
- 2回以上呼ばれる関数のβ簡約はβ展開と呼ぶ
- β展開はコード複製を伴うのでプログラムサイズが増えて効率化になるとは限らない
- β縮約だけ行うことで定数畳み込みを「プログラムサイズと速度を両方向上させる」ようにする

既知のレコードの射影

- 変数 r が RECORD 演算子により静的に束縛されて、r が SELECT 演算子の引数になるなら、SELECT の結果を RECORD の n 番目の引数で置き換えていい
- 原子式を原子式で置き換えるだけなのでプログラムサイズは肥大しない
- 自由変数を増やすのでレジスタ割当が難しくなるので注意

無用変数除去

- 例外送出やストアの更新を伴わない演算子の結果 r がどこでも使用されていないなら、束縛を除去できる
- SML の良いところに意味論が決定的な点がある

引数の平坦化

- タプルのパターンマッチを展開した結果としてレコードをヒープに割り当てるコードが生成される
- CPS 言語にある多引数関数を使う方が最終的に効率がいいコードになる
- 変換によってコードを壊さないように注意
- 略

無用引数除去

- 本体で使用されていない引数は除去できる

SWITCH の定数畳み込み

- 引数 i が定数なら、i 番目の引数に置き換えていい

RECORD 最適化

- レジスタ利用の効率化あるいはレジスタ数の制限のため、レコードは選択パス (SELp) を引数に取れる
- 巨大なレコードがあるとき重要 (ファンクタとかを展開すると発生しがち)

----

----

### 10. クロージャ変換

...

#### クロージャをスタックに割り当てられる条件

ヒープ確保はコストがかかるので可能ならスタックに割り当てたい。(call/cc があると静的解析は不可能になる。) (callee-save-register 最適化によりヒープ確保のコストを下げるとスタック確保への置き換えは相対的に効果が下がって、Standard ML of New Jersey では採用していないらしい。)

- 継続クロージャは常にスタックに割り当てられる
- 関数 f が上にエスケープしないとき、それより上にある継続を呼ぶタイミングで f のスコープから外れるので、解放できる

- 上にエスケープ (escape upward): 関数の結果になる (= 継続の引数になる) か、レコードやセルに格納されること
- 下にエスケープ (escape downward): 他の関数の引数として渡されること

それぞれの関数のパラメータが上にエスケープするか調べておく。上にエスケープするパラメータに渡された値は上にエスケープするとみなす。推移的に解析できる。

クロージャ変換の後、すべての関数は自由変数を含まなくなるから、FIX は1個にまとめられる。(lifting) FIX で定義される関数への参照は VAR ではなく LABEL でよくなる。

### 11. レジスタのスピル

...
