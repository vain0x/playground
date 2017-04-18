namespace MicroStream

open System
open System.Diagnostics
open MicroStream.Data.Entity
open MicroStream.Sources

module Program =
  let tryCodeAsync (url: string) =
    async {
      Process.Start(url) |> ignore
      let! line = Console.In.ReadLineAsync() |> Async.AwaitTask
      return line |> Option.ofObj
    }

  let runAsync database =
    async {
      let instance = "mstdn.jp"
      let userName = "vain0"
      let! client = Mastodon.tryClientAsync database instance userName tryCodeAsync
      match client with
      | Some client ->
        let streaming = client.GetPublicStreaming()
        streaming.OnUpdate |> Event.add
          (fun e ->
            printfn "----------------"
            printfn "@%s" e.Status.Account.AccountName
            printfn "%s" e.Status.Content
          )
        streaming.Start()
        return! Console.In.ReadLineAsync() |> Async.AwaitTask |> Async.Ignore
      | None ->
        ()
    }

  [<EntryPoint>]
  let main argv = 
    try
      let database = AppDatabase()
      runAsync database |> Async.RunSynchronously
      0
    with
    | e ->
      eprintfn "%A" e
      1
