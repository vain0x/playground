module Tests

open System
open System.IO
open RaiiLang.CirDump
open RaiiLang.CirGen
open RaiiLang.KirGen
open RaiiLang.KirClosureConversion
open RaiiLang.SyntaxLower
open RaiiLang.SyntaxParse
open RaiiLang.SyntaxTokenize
open Xunit

let inline is< ^T> (expected: ^T) (actual: ^T) =
  Assert.Equal(actual, expected)

let findTestsDirectory () =
  let cwd = Environment.CurrentDirectory
  Seq.unfold (fun (dir: string) -> let p = Path.GetDirectoryName(dir) in Some (p, p)) cwd
  |> Seq.take 10
  |> Seq.find (fun (dir: string) -> Path.GetFileName(dir) = "2020-01-01-raii-lang")
  |> fun dir -> Path.Combine(dir, "tests")

let snapshotTest (name: string) =
  let testsDir = findTestsDirectory ()

  let sourceName = sprintf "%s/%s/%s.raii" testsDir name name
  if File.Exists(sourceName) |> not then
    false
  else

  let writeLog title ext x =
    let fileName =
      sprintf "%s/%s/%s_%s_snapshot%s" testsDir name name title ext
    let content =
      match box x with
      | :? string as x ->
        x
      | _ ->
        sprintf "%A" x
    File.WriteAllText(fileName, content.TrimEnd() + "\n")

  let tee title ext f x =
    writeLog title ext (f x)
    x

  let sourceCode = File.ReadAllText(sourceName)

  sourceCode
  |> parse
  |> tee "parse" ".txt" nodeToSnapshot
  |> lower
  |> kirGen
  |> kirClosureConversion
  |> cirGen
  |> tee "dump" ".c" cirDump
  |> ignore

  true

[<Fact>]
let testSnapshots () =
  let testsDir = findTestsDirectory ()

  Directory.EnumerateDirectories(testsDir)
  |> Seq.map (Path.GetFileName >> snapshotTest)
  |> Seq.length
  |> fun n -> n >= 1 |> is true
