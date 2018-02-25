namespace VainZero.LyricsParser

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

module Program =
  [<EntryPoint>]
  let main argv =
    match argv with
    | [||] ->
      let solutionDir = Path.GetDirectoryName(Environment.CurrentDirectory)
      let sampleDir = Path.Combine(solutionDir, "samples")
      let errorCount = Tests.run sampleDir
      if errorCount = 0 then 0 else 1
    | _ ->
      0
