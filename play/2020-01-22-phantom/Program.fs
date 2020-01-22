module Program

open System
open Core
open SpaceCheck

[<EntryPoint>]
let main _ =
  spaceCheck ExSignedI64.paramCheckFn
  spaceCheck ExSignedI64.useEnsuresFn
  printfn "OK"
  0
