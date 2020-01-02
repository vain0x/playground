# メモ

## 構文

(予定)

型

```fs
    int
    string

    struct Pair {
        x: int
        y: int
    }
```

変数

```fs
    let x = ...;
    let mut y = ...;
    let mut z = ...;
```

関数

```fs
    fn write_line(in s: string) {
        print(s);
        print("\n");
    }

    write_line("hello");

    fn assign(ref x: string, y: string) {
        x = y;
    }

    assign(ref x, move y);
```

条件分岐

```fs
    cond match {
        p1 => e1;
        p2 => e2;
        ...
    }

    x && y

    cond then {
        body
    } else {
        alt
    }
    // ==> cond match { true => body; false => alt }

    cond else {
        alt
    }
    // ==> cond then x => { x } else { alt }

    cond then x => {
        body(x)
    } else {
        alt
    }
    // ==> cond match { Some(x) => body(x); None => alt }

    cond then x => {
        body(x)
    } else e => {
        alt(e)
    }
    // ==> cond match { Ok(x) => body(x); Err(e) => alt(e) }
```

ループ

```fs
    break

    continue

    loop {
        body
    }

    cond while {
        body
    }
    // ==> loop { cond then { body } else { break } }

    cond while x => {
        body(x)
    }
    // ==> loop { cond then x => { body(x) } else { break } }

    xs for x => {
        body(x)
    }
    // ==> {
    //     let mut i = xs.into_iter();
    //     loop {
    //         i.next() match {
    //             Some(x) => body(x);
    //             None => break;
    //         }
    //     }
    // }
```

ジェネリクス

```fs
    meta(T)
    fn last(ref array: Array[T]) -> ref T {
        array.is_empty() then {
            null
        } else {
            array[array.len() - 1]
        }
    }

    let mut a = [1, 2, 3];
    // T=int
    last(ref a) += 10;
    assert(a[0] == 11);
```

## モード

- own?: 所有するか否か。
- ptr?: ポインタ越しに (参照として) 保持する否か。
- read?: 読み取り可能か否か。
- write?: 書き込み可能か否か。

| own?  | ptr?  | read? | write?    | Rust              | raii-lang         |
|:------|:------|:------|:----------|:------------------|:------------------|
| Yes   | Yes   | Yes   | Yes       | `mut x: Box<T>`   | `box mut x`       |
| Yes   | Yes   | Yes   | No        | `x: Box<T>`       | `box [in] x`      |
| Yes   | Yes   | No    | Yes       | --                | `box out x`       |
| Yes   | Yes   | No    | No        | --                | --                |
| Yes   | No    | Yes   | Yes       | `mut x: T`        | `mut x`           |
| Yes   | No    | Yes   | No        | `x: T`            | `x`               |
| Yes   | No    | No    | Yes       | --                | `out`             |
| Yes   | No    | No    | No        | --                | --                |
| No    | Yes   | Yes   | Yes       | `x: &mut T`       | `ref mut x`       |
| No    | Yes   | Yes   | No        | `x: &T`           | `ref x`           |
| No    | Yes   | No    | Yes       | --                | `ref out x`       |
| No    | Yes   | No    | No        | --                | --                |
| No    | No    | Yes   | Yes       | `mut x: C`        | `mut x: C`        |
| No    | No    | Yes   | No        | `x: C`            | `x: C`            |
| No    | No    | No    | Yes       | --                | `out x: C`        |
| No    | No    | No    | No        | --                | --                |

- T は型。
- C は Copy トレイトを満たす型 (i64 など)。
- x は変数。
