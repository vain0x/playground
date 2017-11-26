# temali

- Razor ライクな軽量な構文
- 構文木を出力
    - コードに html エスケープなどの処理を埋め込むとスクリプト言語的になってしまう
    - エスケープが正しくなされているか確認するために静的解析を用意するのも二度手間。
    - ホスト言語が解釈することにより自由度を高める。
    - 構文に固有のセマンティクスがない。XML みたいなもの。
    - 複数行文字列を扱いやすいデータ記述言語？

```
- if の構文
    Razor 風にしたかったが、else の解析と波括弧のエスケープがだるそう。
    @if is_empty(list) { <p>No results.</p> }

    波括弧を2重にするという案もあった。括弧を用いることでテキストエディターの括弧の対応を検出する機能の支援を受けようと思ったが、対象言語の括弧と互い違いになってむしろ相性が悪い可能性もあるのでやめた。

    コロン-end 方式
    if の後にスペースがあると出力式と区別がつかないという問題があるが、if などを構文的に特別扱いするという方法で対処。
    @if is_empty(list): @else: @end
```

```
@module main {{
    @meta file_name = "sample.html";
    @meta language = "html";

    @require html_helper;

    @filter match {{
        @case (html, source) {{ source }}
        @case value {{
            @throw { reason: "Don't render non-html values.", value: value };
        }}
    }}
}}

<!doctype html>
<html>
    <body>
        @for name in names {{
            <p>
                Hello, @name;!
            </p>
        }}

        @match model.age_type {{
            @case "young" {{
                Price: $199.99
            }}
            @case "adult" {{
                Price: $299.99
            }}
        }}
    </body>
</html>
```

```
@*
複数行コメント
*@

@!x
    エスケープ (x を強制的に対象言語側にする)
    @!@
    @!;
    @!{{
    @!}}


@<invocation-expression>
    a.b.c() のような式
    (x <- a) のようにスペースを含む式は書けない。

@<invocation-expression>: <target-document> @end
    式を

@(<expression>)
    任意の式


@rule (head) {{

}}


@runtime(htmlpp, 1.0)
@require(html_helpers)

<!doctype html>
<html>
    <body>
        @if is_empty(list):
            @fun f(x):
            @end
        @else:
        @end

        @let print_items(items):
            @if items.is_empty.not:
                <ol>
                    @for item in items:
                        <li>@item</li>
                        @if item.has_error: @break
                    @end
                </ol>
            @else:
                <p>No items.</p>
            @end

            @expand(items, item):
                (
                    @body:
                        @item
                    @end
                    @delimiter:
                        ,
                    @end
                )
            @else:
                "No items."
            @end
        @end

        @let print_items_rec(list):
            @let go(i):
                @if i < list.length:
                    <li>@list(i)</li>
                    @go(i + 1)
                @end
            @end
            @go(0)
        @end
        @do print_items_rec(model.list)

        @do imperative:
            @bind x: @xs
            @bind y:
                @fun
                @end
            @end
            @yield x
        @end
    </body>
</html>

(doc
    (text """<!doctype html>
<html>
    <body>""")
    (for
        (model "names")
        (atom "name")
        (doc
            (text """
        <p>
            Hello, """)
            (var "name")
            (text "!")
        )
        (text """
            </p>
        """)))
```

```
@songs:
    @song:
        @title: Hey Jude
        @artist: The Beetles
        @lyrics:
            Hey Jude Don't make it bad
            ...
        @end
    @end
@end
```

```
@ul:
    @li: a
    @li: b
    @li: c
@end
```

```
temali spec

temali-lang: 構文仕様
temali-runtime: temali 言語を解釈して何かを行なうプログラム

<document>
    ドキュメント。任意のテキストと temali 挿入句からなる部分。
    コロン : の後から改行までの間に空白でもコメントでもない字句があれば、その行が中身。
    なければ対応する @end の直前までが中身。
    (block_document nodes...)
    temali 文書全体は <document-multiline> である。

<term>
    項。
    識別子、ナビゲーション、文字列、数値、リスト、レコード、括弧項。
    @ の後ろにおくとき、末尾を指定しなくてもいい。

<expr>
    式。
    有界項、複合項、二項演算式、if 式、match 式、let 式。
    1 + 2 のように、括弧で括らなければ範囲が分からない。

<keyword>
    予約語。 @ の直後では、生識別子としては解釈されない。

@* <comment:c> *@
    (block_comment c)

@let <term:y> = <term:x>
    ordinal let block
    (block_let y (some x) none)

@let <term:y> = <term:f>: <document:d> @end
    complex let block
    (block_let y (some f) (some d))

@let <term:y>: <document:d> @end
    simple let block
    (block_let y none (some d))

@do <term:x>
    (block_do x)

@do <term:x>: <document:d> @end
    (block_do x d)

@if <term:p>: <document:t> [@else if: <term:p_i> <document:d_i> @end]* [@else: <document:e>] @end
    if block
    (block_if p t (list (p_i, d_i)*) (option e?))

@match
    未実装

@for <term:x> in <term:xs>: <document:d> @end
    for ブロック
    (for x xs d)

@while <term:p>: <document:d> @end
    未実装

@(<expr:t>)
    (block_term t)
    丸括弧の直後からドキュメントの一部として扱われる。

@<term:t>
    (block_term t)
    @f の直後に対象言語の a を置くことはできない (@fa となってしまう) が、そのときは @(f)a とする。

@<expr:f>(<expr:x>)
    (block_term (term_app f x))

@<term:f>: <document:d> @end
    (block_term (term_app f d))
```

```
Typing

<type-term>
    a
    a[<type-expr>]

<type-expr>
    a
    some a | none
    { name: string, value: a }

<term-typed>
    <term> !<type-term>

```
