module Program

open System
open FSharp.Text.Lexing

[<EntryPoint>]
let main _ =
  let input = stdin.ReadToEnd()

  let lexbuf = LexBuffer<_>.FromString input

  let ast =
    // let tokens = ResizeArray()

    // 空白トークンを飛ばされるから位置情報をうまく計算できない
    // パース中の行番号、列番号 (0-index)
    // let mutable row = 0
    // let mutable column = 0
    // let debugPos (row, column) = sprintf "%d:%d" (row + 1) (column + 1)

    let read (b: LexBuffer<char>) =
      let t = Lexer.read b

      // デバッグ用にトークン配列と位置情報をメンテナンスする
      do
        // let start = row, column

        // token text
        let tt = String(b.Lexeme)
        // let index = tt.LastIndexOf('\n')
        // if index < 0 then
        //   column <- column + tt.Length
        // else
        //   // 行番号を更新する
        //   for i in 0..index do
        //     if tt.[i] = '\n' then
        //       row <- row + 1

        //   // 列位置 = 最終行の長さ
        //   column <- tt.Length - (index + 1)

        // let last = row, column
        // tokens.Add((t, tt, start, last))
        // eprintfn "%d:%d %A %A" (fst start + 1) (snd start + 1) t tt
        // eprintfn "read %A" tt
        ()

      t

    try
      Parser.prog read lexbuf
    with
    // `System.Exception: parse error` is thrown from FsYacc on parse error.
    // | ex when ex.Message.Contains("parse error") ->
    //   // let dp (p: Position) = sprintf "%d:%d" (p.Line + 1) (p.Column + 1)
    //   // let p = lexbuf.StartPos // always Line = 0
    //   // eprintfn "Parse error at %A-%A: %s" (dp lexbuf.StartPos) (dp lexbuf.EndPos) ex.Message

    //   // eprintfn "Parse error at %s: %A" (debugPos (row, column)) ex
    //   exit 1
    | _ -> reraise ()

  printfn "%A" ast
  0
