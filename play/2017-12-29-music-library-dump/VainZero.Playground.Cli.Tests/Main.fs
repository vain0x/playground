module CliTest

open Expecto

[<EntryPoint>]
let main argv =
  Tests.runTestsInAssembly defaultConfig argv
