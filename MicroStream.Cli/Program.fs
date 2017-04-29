namespace MicroStream

open System
open System.Diagnostics
open MicroStream
open MicroStream.Data.Entity
open MicroStream.Sources

module Program =
  let authenticator =
    { new IAuthenticator with
        override this.AuthenticateAsync(uri: Uri) =
          async {
            printfn "Authenticate on the webpage and copy the code you got:"
            Process.Start(uri |> string) |> ignore
            let! line = Console.In.ReadLineAsync() |> Async.AwaitTask
            return line |> Option.ofObj
          }
    }

  let printPost (post: Post) =
    printfn "----------------"
    printfn "%s (@%s)" post.Publisher.DisplayName post.Publisher.UserName
    printfn "%s" post.Content

  let runAsync database =
    async {
      let instance = "pawoo.net"
      let userName = "vain0"
      let! client = Mastodon.tryClientAsync database authenticator instance userName
      let! twitterClient = Twitter.tryClientAsync database authenticator "ue_dai"
      match (client, twitterClient) with
      | (Some client, Some twitterClient) ->

        // Twitter
        let! twitterUser =
          Tweetinvi.UserAsync.GetAuthenticatedUser(twitterClient) |> Async.AwaitTask
        let twitterStream = Tweetinvi.Stream.CreateUserStream(twitterClient)
        twitterStream.TweetCreatedByAnyone |> Event.add
          (fun e -> e.Tweet |> TwitterPost |> printPost)
        twitterStream.StartStream()

        // Mastodon
        let streaming = client.GetPublicStreaming()
        streaming.OnUpdate |> Event.add
          (fun e -> e.Status |> MastodonPost |> printPost)
        streaming.Start()

        return! Console.In.ReadLineAsync() |> Async.AwaitTask |> Async.Ignore
      | _ ->
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
