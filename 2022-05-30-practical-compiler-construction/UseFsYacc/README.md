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

testsにあるファイルはすべてパースできる。ただし結果の検証はしていない

## 衝突

このプロジェクトをコンパイルするとぶら下がりif文の衝突について指摘される (動作上の問題はない)

> shift/reduce error at state 57 on terminal ELSE between {noprec shift(58)} and {noprec reduce(stmt:'IF' 'LP' cond 'RP' stmt)} - assuming the former because we prefer shift when unable to compare precedences
