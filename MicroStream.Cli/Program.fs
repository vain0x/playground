namespace MicroStream

open System
open System.Diagnostics
open Mastonet
open Mastonet.Entities

module Program =
  let createApp server =
    async {
      let! app =
        MastodonClient.CreateApp
          ( server
          , "MicroStream"
          , Scope.Read ||| Scope.Write ||| Scope.Follow
          , "https://github.com/vain0/MicroStream"
          ) |> Async.AwaitTask
      printfn
        """
        AppRegistration
          ( Instance = "%s"
          , ClientId = "%s"
          , ClientSecret = "%s"
          )
        """ app.Instance app.ClientId app.ClientSecret
      Console.ReadLine() |> ignore
    }

  let authorize app =
    async {
      let client = MastodonClient(app)
      let url = client.OAuthUrl()
      Process.Start(url) |> ignore
      let code = Console.ReadLine()
      let! auth = client.ConnectWithCode(code) |> Async.AwaitTask
      printfn "%s" auth.AccessToken
      Console.ReadLine() |> ignore
    }

  let watch app accessToken =
    async {
      let client = MastodonClient(app, accessToken)
      let stream = client.GetPublicStreaming()
      stream.OnUpdate.AddHandler(fun _ e -> printfn "%s said: %s" e.Status.Account.AccountName e.Status.Content)
      stream.Start()

      while Console.ReadLine() |> String.IsNullOrEmpty |> not do
        ()
    }

  let app =
    AppRegistration
      ( Instance = "pawoo.net"
      , ClientId = "35a603837616a7b3e274312fda9954b9a49c09a85db92c920797abaac3fb7055"
      , ClientSecret = "470cbb8166323dfec4d380a503f9d2774e4910301408bfa5cf1a954caecd5121"
      )

  let accessToken =
    "35bde46ba939bc2b97af021673dab846c00400540d34eebb4a7df0620b7e56b8"

  [<EntryPoint>]
  let main argv =
    watch app accessToken
    |> Async.RunSynchronously
    0
