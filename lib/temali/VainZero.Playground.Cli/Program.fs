namespace VainZero.Playground

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Net
open System.Reflection
open System.Web
open System.Text
open System.Threading

open VainZero.Temali.Syntax

module Program =
  [<EntryPoint>]
  let main argv =
    let result = Parsing.tryParse " a " Parsing.inputParser
    printfn "%A" result
    0
