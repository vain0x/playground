module DeriveGen.ExampleTests

open System.IO
open Persimmon
open UseTestNameByReflection

module Result =
  let get =
    function
    | Result.Ok value -> value
    | Result.Error error -> failwithf "%O" error

module ProcessorTests =
  let ``my test`` = test {
    do! assertPred true
  }

  //(*
  let ``test generate`` = test {
    let exampleDir = Path.GetFullPath(@"../../../../examples")
    let inputFiles = Directory.GetFiles(exampleDir, "*-input.json")
    do! assertPred (inputFiles.Length > 0)
    let () =
      for inputFile in inputFiles do
        let outputFile = inputFile.Replace("-input.json", "-output.g.cs")
        let inputJson = File.ReadAllText(Path.Combine(exampleDir, inputFile))
        let output =
          DeriveGen.Processor.generate inputJson
          |> Result.get
        File.WriteAllText(Path.Combine(exampleDir, outputFile), output)
    return ()
  }
  //*)

module Utf8JsonTests =
  open Utf8Json

  type Bar = {
    BarName: string
  }

  type Foo = {
    FooName: string
    Bars: Bar[]
  }

  let ``test serialize json to objects`` = test {
    let foo =
      {
        FooName = "foo"
        Bars =
          [|
            { BarName = "bar1" }
            { BarName = "bar2" }
          |]
      }
    let data = JsonSerializer.Serialize(foo)
    let json = JsonSerializer.PrettyPrint(data).Replace("\r\n", "\n")
    let expected =
      """{
  "FooName": "foo",
  "Bars": [
    {
      "BarName": "bar1"
    },
    {
      "BarName": "bar2"
    }
  ]
}"""
    do! json |> assertEquals (expected.Replace("\r\n", "\n"))
  }

module ChironTests =
  open Chiron
  open Chiron.Inference

  module D = Json.Decode

  type Foo = {
    Name: string
    Age: int
  }
  with
    static member FromJson(_: Foo) =
      jsonDecoder {
        let! name = D.required "name"
        let! age = D.required "age"
        return { Name = name; Age = age }
      }

  let ``test decode`` = test {
    let source = """{ "name": "John Doe", "age": 42 }"""
    let result =
      Json.parse source
      |> JsonResult.bind Json.decode
      |> JsonResult.toResult
    do! result |> assertEquals (Ok { Name = "John Doe"; Age = 42 })
  }

  type NamespaceModel = {
    Map: Map<string, string>
  }
  with
    static member FromJson(_: NamespaceModel) =
      let decodeObject =
        D.mapWith D.string
        |> Decoder.map (fun map -> { Map = map })
      let decodeArray =
        D.arrayWith D.string
        |> Decoder.map (fun names ->
          names
          |> Array.map (fun name -> (name, name))
          |> Map.ofArray
          |> fun map -> { Map = map }
        )
      let decodeString =
        D.string
        |> Decoder.map (fun name -> { Map = Map.ofList [(name, name)] })
      D.oneOf [
        decodeObject
        decodeArray
        decodeString
      ]

  let ``test decode complex object`` =
    let body (source, expected) = test {
      let actual =
        source
        |> Json.parse
        |> JsonResult.bind Json.decode
      do! actual |> assertEquals (JsonResult.pass ({ Map = expected }))
    }
    parameterize {
      case ("\"foo\"", Map.ofList[("foo", "foo")])
      case ("[]", Map.empty)
      case (
        """["foo", "bar"]""",
        Map.ofList [
          ("foo", "foo")
          ("bar", "bar")
        ])
      case ("{}", Map.empty)
      case (
        """{ "foo": "F", "bar": "B" }""",
        Map.ofList [
          ("foo", "F")
          ("bar", "B")
        ])
      run body
    }
