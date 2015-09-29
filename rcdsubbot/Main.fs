module Main

open FSharp.Compatibility.OCaml.Format
open FSharp.Compatibility.OCaml.Core
open Support.Pervasive
open Support.Error

[<EntryPoint>]
let main argv =
  let exprs =
    [| "unit" 
    ;  "lambda x:Top.x"
    ;  "(lambda x : Top -> Top. x) (lambda z: Top. z)"
    ;  "(lambda x: Top. x) (lambda x: Top. x)"
    ;  "{ x = unit }"
    ;  "{x=unit      , y = lambda x: Top. x}"
    ;  "lambda r: { }. {}"
    ;  "lambda r: { x: Top }. { x = unit }"
    ;  "{x=unit}.x"
    ;  "lambda x: Bot. x"
    ;  "lambda x: Bot. x x"
    ;  "lambda x: Bot. x.y x.y"
    ;  "(lambda r: { x: Top->Top }. r) {x = lambda z: Top. z}"
    ;  "(lambda r: { x: Top->Top }. r.x r.x) {x = lambda z: Top. z, y = lambda z: Top. z}"
    ;  "(lambda r: {x: Unit}. r.x) {y = unit, x = unit}"
    |]
  
  exprs |> Array.iter (fun s ->
    let (t, ctx) = parseExpr "rcdsubbot" s
    printtm ctx t
    print_newline ()
    let tyT = t |> typeof ctx
    printty tyT
    print_newline ()
    pr "-----------------------"
    print_newline ()
    ()
  )
  do print_flush ()
  0
