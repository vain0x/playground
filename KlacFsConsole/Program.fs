// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open KlacFs
open Basis.Core

[<EntryPoint>]
let main argv =

    let klac = new KlacFs()
    let continues = ref true

    while !continues do
        let s = Console.ReadLine()
        if s = null || s = "#quit" then
            continues := false
        else
            let output =
                match klac.TryParse s with
                | Success ex ->
                    ex |> klac.Inspect
                | Failure msg -> msg
            Console.WriteLine output

    printfn "%A" argv
    0 // return an integer exit code
