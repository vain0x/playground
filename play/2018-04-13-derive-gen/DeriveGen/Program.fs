namespace DeriveGen

module Program =
  open System

  [<RequireQualifiedAccess>]
  type ExitCode =
    | Ok = 0
    | Error = 1

  let viaStandardIO () =
    async {
      let! json =  Console.In.ReadToEndAsync() |> Async.AwaitTask
      match json |> Processor.generate with
      | Ok csharpCode ->
        do! Console.Out.WriteAsync(csharpCode) |> Async.AwaitTask
        return ExitCode.Ok
      | Error err ->
        let err = err |> Chiron.JsonFailure.summarize
        do! Console.Error.WriteAsync(err) |> Async.AwaitTask
        return ExitCode.Error
    }

  [<EntryPoint>]
  let main _ =
    viaStandardIO ()
    |> Async.RunSynchronously
    |> int<ExitCode>
