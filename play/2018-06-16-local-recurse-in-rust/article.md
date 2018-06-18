クロージャを再帰呼び出しする方法を考えました。

競技プログラミングではローカル変数を書き換えながら再帰する処理がよく出てきます。しかし Rust でそれを書こうとするとやや冗長になりがちです。

本稿では小さなヘルパーを用意して記述を簡略化することを試みました。

- 環境: Rust 1.15.1 (AtCoder での現在のバージョン)
- 筆者: AtCoder もうすぐ青といい続けて1年

## 要約

- 競プロではよく再帰する
- 小さなアダプタを書くと再帰呼び出しできる
- イミュータブルなクロージャはローカル変数を書き換えられない
    - `RefCell` で対処する
- 成果:
    - [Fn で再帰するやつ](https://play.rust-lang.org/?gist=97ad8427affee25a31656d750d2a01d6&version=stable&mode=debug)

## 用例1: 階乗

単純な例として、階乗の計算を再帰で書けるようにしましょう。内部で自身を参照するために、クロージャは引数に `fact` (階乗関数) を受け取るようにする方針でいきます。

```rust
    let fact_5 = recurse(5, &|n, fact| {
        if n <= 1 {
            1_i64
        } else {
            n * fact(n - 1)
        }
    });
    assert_eq!(1 * 2 * 3 * 4 * 5, fact_5);
```

ここで ``recurse(x, f)`` が ``f(x, f)`` の意味になるように後で定義するヘルパーです。

「なぜ作った関数を即座に起動するのか」という疑問があると思いますが、それは実際にそういう用途が多いからです。再帰関数がほしいときは ``|x| recurse(x, &|x, f| ..)`` のようにクロージャ化する運用でも大丈夫でしょう。

## 実装1: イミュータブル版

`recurse` の実装は簡単で、`fn` で定義した関数が再帰可能であることを利用します。

```rust
    fn recurse<X, Y>(x: X, f: &Fn(X, &Fn(X) -> Y) -> Y) -> Y {
        f(x, &|x: X| recurse(x, &f))
    }
```

注意点は、クロージャの引数の型がまたそのクロージャの型で……という無限の循環を避けるため、関数を [トレイトオブジェクト](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/trait-objects.html) への参照という形で扱っていることです。

``Fn(X) -> Y`` というのは「型 `X` の値を受け取って型 `Y` の値を返す関数」の型を表すトレイトで、ある種のクロージャは自動的に `Fn` を実装した型になります。参照: [std::ops::Fn - Rust](https://doc.rust-lang.org/std/ops/trait.Fn.html)

    &Fn(X, &Fn(X) -> Y) -> Y
            ^^^^^^^^^^           再帰関数の型 (クロージャの引数)
     ^^^^^^^^^^^^^^^^^^^^^^^     定義したクロージャのトレイトオブジェクトの型

## 用例2: DFSで連結成分分解

次に現実的な例として、グラフの連結成分分解を深さ優先探索で書いてみます。

```rust
    //
    //   0 -- 1
    //   | \
    //   |  \
    //   2 -- 3    4--5
    //
    let graph =
        vec![
            vec![1, 2, 3],
            vec![0],
            vec![0, 3],
            vec![0, 2],
            vec![5],
            vec![4],
        ];
    let n = graph.len();

    let roots = RefCell::new(vec![n; n]);
    for u in 0..n {
        recurse(u, &|v, go| {
            if roots.borrow()[v] < n {
                return;
            }

            roots.borrow_mut()[v] = u;

            for &w in graph[v].iter() {
                go(w);
            }
        })
    }

    assert_eq!(&*roots.borrow(), &[0, 0, 0, 0, 4, 4]);
```

頂点 `v` が属す連結成分の代表を ``roots[v]`` に入れていきます。

このとき、再帰の途中で配列を更新する必要があります。しかし `roots` を let **mut** でミュータブル配列として宣言すると、先ほどの `recurse` は使えません。というもの、外部のミュータブルな変数を借用するクロージャは `Fn` トレイトを実装しないからです。

ここでは `RefCell` を使ってこの問題を回避しています。クロージャに渡すのが `RefCell` へのイミュータブルな参照でも、内部の値をミュータブルとして扱えます。参照: [保証を選ぶ](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/choosing-your-guarantees.html#refcellt)

なんにせよ、それなりに簡潔に再帰処理ができました！

- [Rust Playground で試す](https://play.rust-lang.org/?gist=97ad8427affee25a31656d750d2a01d6&version=stable&mode=debug)

## 実装2. ミュータブル版

**追記**: ミュータブルなローカル変数を書き換えながらクロージャを再帰呼び出しする方法について記述していましたが、 [安全でないコードが書けてしまう](https://qiita.com/vain0x/items/90c9580aa34926160ac1#comment-1988da50c4701cc0add8) ので取り下げました。

<details>
<summary>変更前の内容はたたんであります。</summary>
<div>
記述量を減らすのが目的なので、 `RefCell` をなくす方法も考えてみます。

クロージャの型が自動で実装するトレイトは `Fn` のほかに `FnMut` もあります。`FnMut` は、簡単にいうと「ミュータブルな状態を持つ関数」の型が実装すべきトレイトです。参照: [std::ops::FnMut - Rust](https://doc.rust-lang.org/std/ops/trait.FnMut.html)

外部のミュータブルな状態 (例えば ``let mut roots = ...``) を触りながら再帰できるように、クロージャが `FnMut` でもいいようにしてみます。すると、借用検査が **通りません** 。

通せるようにしたのが以下です:

```rust
fn recurse<X, Y>(x: X, f: &mut FnMut(X, &mut FnMut(X) -> Y) -> Y) -> Y {
    let fp = f as *mut FnMut(X, &mut FnMut(X) -> Y) -> Y;
    let f1 = unsafe { &mut *fp };
    let f2 = unsafe { &mut *fp };
    f1(x, &mut |x: X| recurse(x, f2))
}
```

これをみると分かるように、 `recurse` は受け取ったクロージャへの参照を2つに複製します: 即座に呼び出すための参照と、再帰用に呼び出すための参照です。ミュータブルな参照は複製できないので、`unsafe` を使って強制的に複製しています。

「unsafe だから危険じゃないのか」という疑問がありますが、実行中のクロージャが自分への参照を self, f で2重に受け取っているだけなので、たぶん大丈夫です。

これで深さ優先探索の例を書き直すと、`RefCell` が消失してすっきり。

```rust
    let mut roots = vec![n; n];
    for u in 0..n {
        recurse(u, &mut |v, go| {
            if roots[v] < n {
                return;
            }

            roots[v] = u;

            for &w in graph[v].iter() {
                go(w);
            }
        })
    }
```

[Rust Playground で試す](https://play.rust-lang.org/?gist=bceca5a2af42a5436996b99712cb28ed&version=stable&mode=debug)
</div>
</details>

## 参考

- [Stebalien commented on 28 Jan 2016](https://github.com/Hoverbear/rust-rosetta/issues/450#issuecomment-175848086)

    Zコンビネータを使ってクロージャを再帰可能にするコードの例。引数として受け取る再帰関数の型は推論されないっぽい。

- [無名再帰 - Google 検索](https://www.google.co.jp/search?q=無名再帰&oq=無名再帰)

    クロージャのような匿名の関数を再帰呼び出しすることを無名再帰というらしい。
