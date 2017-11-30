open System
open KlacFs
open Basis.Core
open Util

module Console =
    let ReadLines () =
        Seq.initInfinite (fun _ -> Console.ReadLine ())
        |> Seq.takeWhile ((<>) null) 
        |> Str.join "\r\n"

let klac =
    new KlacFs()

let epl s =
    let output =
        match klac.TryParse s with
        | Success ex ->
            ex |> klac.Inspect
            //TODO: eval
        | Failure msg -> msg
    Console.WriteLine output

let proc_command = function
    | null -> exit 0
    | "" -> epl <| Console.ReadLines()
    | s  -> epl s

let rec repl () =
    proc_command <| Console.ReadLine()
    repl ()
    
[<EntryPoint>]
let main argv =
    printfn "KlacFs REPL"
    repl ()
