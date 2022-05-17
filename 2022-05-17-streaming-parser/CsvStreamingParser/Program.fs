open System
open System.IO

type Parser =
  { Row: int
    Column: int
    Flags: int
    Item: string }

type Handler =
  { // row, column, value
    OnItem: int * int * string -> unit
    // end of current item
    OnNext: unit -> unit }

let private newParser () : Parser =
  { Row = 0
    Column = 0
    Flags = 0
    Item = "" }

let private feed (h: Handler) (chunk: string) (p: Parser) : Parser =
  // any = [^]
  // non-quote = [^"]
  // non-meta = [^,"\r\n]
  // item = ('"' ('""' | non-quote)* '"') | non-meta*
  // contents = BOM? ((item (',' item)*)? EOL)*

  let rec go flags item row column i =
    if i < chunk.Length then
      match chunk.[i] with
      | '"' when (flags &&& 8) <> 0 -> go 4 (item + "\"") row column (i + 1) // escaped quote
      | '"' when (flags &&& 4) <> 0 -> go 8 item row column (i + 1) // 8: after quote in quote

      | '"' when (flags &&& 4) = 0 -> go 4 "" row column (i + 1) // 4: in quote
      | c when (flags &&& 4) <> 0 -> go 4 (item + string c) row column (i + 1) // continue quote

      | ',' ->
        h.OnItem(row, column, item)
        go 2 "" row (column + 1) (i + 1) // 2: emit OnItem even if next is empty

      | '\r' ->
        h.OnItem(row, column, item)
        h.OnNext()
        go 1 "" (row + 1) 0 (i + 1) // 1: eat LF

      | '\n' when (flags &&& 1) <> 0 -> go 0 item row column (i + 1)

      | '\n' ->
        h.OnItem(row, column, item)
        h.OnNext()
        go 0 "" (row + 1) 0 (i + 1)

      | c -> go 0 (item + string c) row column (i + 1)
    else
      row, column, flags, item

  let row, column, flags, item = go p.Flags p.Item p.Row p.Column 0

  { Row = row
    Column = column
    Flags = flags
    Item = item }

let finish (h: Handler) (p: Parser) : unit =
  let row, column, flags, item = p.Row, p.Column, p.Flags, p.Item

  if (flags &&& 2) <> 0 then
    h.OnItem(row, column, item)
    h.OnNext()
  else
    assert (item.Length = 0) // flags 2 is on only immediately after ,

[<EntryPoint>]
let main _ =
  let h: Handler =
    { OnItem = fun (row, column, item) -> printfn "push %A (%d:%d)" item row column
      OnNext = fun () -> printfn "commit" }

  let input = stdin.ReadToEnd()
  let mutable p = newParser ()

  for c in input do
    p <- feed h (string c) p

  finish h p
  0
