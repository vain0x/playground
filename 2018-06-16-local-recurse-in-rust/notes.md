

- メモ化再帰するとき長くなりがちなメモの型を書かなくて済む
- 関数を返す関数にするより `recurse` のほうが引数の型推論が効きやすいっぽい
- 実プロダクトなら `struct` と `impl` をローカルに定義するほうがよい
- 連結成分分解は Union-Find のほうが簡単




目標: ローカル変数を参照しながら再帰できるクロージャを軽量な構文で定義する。
理由: 競プロで頻出だから

- ユースケース: メモ化fib, 連結成分分解(dfs)

- バージョン: 1.15 stable
- クロージャ: 参照を持ちまわる必要がない、型注釈もいらない、ただし再帰ができない
- 解決1: Zコンビネーターと RefCell を使う
    - 持ちまわるべき状態をクロージャーの外部で RefCell にいれて用意する
    - RefCell まわりがうるさい (RefCell::new, .borrow など)
- 解決2: RefCell ではなく unsafe を使う
    - クロージャに間接的にそれ自身への参照を渡すことになる
    - 再帰関数を起動する側とクロージャ自身がクロージャへの可変参照を持つので借用ルールに違反
    - unsafe を使って可変参照を複製する、実際問題はない
        - 実際問題はないはず
        - クロージャの所有権は recurse が持ち続ける
        - 内部でナゾの可変参照が増殖するが、recurse の中で消費される
    - 簡潔に書ける。こっちを使おう。

- 余談1: fn: 関数の中にローカルに書ける、再帰できる、実行時の環境を持たないので参照を引数で持ちまわる必要があってだるい、型注釈必須なのもだるい
- 余談2: struct + impl: 再帰できる、参照を self で書けるので短い気がする
- 余談3: unsafe way
    - クロージャに自己参照を持たせればいい
    - ヒープ上にバッファを確保して、それへの参照をクロージャに持たせて、そのバッファ上にクロージャを移動する
    - 再帰関数を起動する側と再帰関数自身が再帰関数への可変参照をもたないといけないので借用のルールに抵触する
        - 簡単にいうと ``g(g, x)``、 `g` への参照が2ついる、可変参照だとダメ
        - 可変参照の一意性: オブジェクトがいつのまにか変わってる、という事態を防ぐ
    - 実際に可変参照が同時に2個所から触られることはないので unsafe で対処できる
- 余談4: マクロ
    - マクロで struct + impl を定義する?
    - 式から型を取り出せない?





```rust
    struct RecursiveFnMut<'a, X: 'a, Y: 'a> {
        f2: &'a mut for<'r> FnMut(X, &'r mut (FnMut(X) -> Y + 'r)) -> Y,
    }

    // impl FnMut(X) -> Y for ..
    impl<'a, X, Y> RecursiveFnMut<'a, X, Y> {
        fn call_mut(&mut self, x: X) -> Y {
            recurse(x, self.f2)
        }
    }
```