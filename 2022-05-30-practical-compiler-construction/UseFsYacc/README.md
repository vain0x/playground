# UseFsYacc

F# 用のパーサジェネレータである [FsYacc](https://github.com/fsprojects/FsLexYacc) を使ってパーサを作る

## 使いかた

標準入力にソースコードを入力する
標準出力に構文木が出力される

1つのファイルをパースする:

```sh
cat tests/fizz_buzz.simple | dotnet run --project UseFsYacc
```

tests以下をすべてパースする:

```sh
find tests -type f -name '*.simple' | \
    xargs -I{} sh -c "cat {} | dotnet run --project UseFsYacc"
```

## 状況

- tests以下はすべてパースできる。結果は検証していない
- 演算子優先度は未実装
