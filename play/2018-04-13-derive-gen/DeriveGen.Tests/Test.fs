module DeriveGen.ExampleTests

open System.IO
open Persimmon
open UseTestNameByReflection

let ``my test`` = test {
  do! assertPred true
}

let ``test generate`` = test {
  let exampleDir = Path.GetFullPath(@"../../../../examples")
  let inputFiles = [|"sample-input.json"|]
  let () =
    for inputFile in inputFiles do
      let outputFile = inputFile.Replace("-input", "-output")
      let inputJson = File.ReadAllText(Path.Combine(exampleDir, inputFile))
      let output = DeriveGen.Processor.generate inputJson
      File.WriteAllText(Path.Combine(exampleDir, outputFile), output)
  return ()
}
