namespace MicroStream

open System
open System.Diagnostics
open System.Reactive.Disposables
open System.Reactive.Linq
open System.Reactive.Threading.Tasks
open System.Threading.Tasks
open FSharp.Control.Reactive
open MicroStream
open MicroStream.Data.Entity
open MicroStream.Reactive.Bindings
open MicroStream.Sources
open MicroStream.StreamViews
open global.Reactive.Bindings

type MainView(database, authenticator, createStreamView) =
  let streamView = Behavior.create StreamView.Empty

  let start () =
    async {
      let! sv = createStreamView ()
      streamView.Value <- sv
    } |> Async.Start

  member this.Authenticator = authenticator

  member this.StreamView = streamView

  member this.Start() = start ()

  member this.Dispose() =
    this.StreamView.Dispose()

  interface IDisposable with
    override this.Dispose() = this.Dispose()

  static member Create(authenticator: IAuthenticator) =
    let database = AppDatabase()
    let createStreamView () =
      async {
        let! mastodonClient = Mastodon.tryClientAsync database authenticator "pawoo.net" "vain0"
        let! twitterClient = Twitter.tryClientAsync database authenticator "ue_dai"
        match (mastodonClient, twitterClient) with
        | (Some mastodonClient, Some twitterClient) ->
          // Twitter
          let! twitterUser =
            Tweetinvi.UserAsync.GetAuthenticatedUser(twitterClient) |> Async.AwaitTask
          let twitterStream =
            { new IObservable<_> with
                override this.Subscribe(observer) =
                  let stream = Tweetinvi.Stream.CreateUserStream(twitterClient)
                  let subscription =
                    stream.TweetCreatedByAnyone
                    |> Observable.subscribeObserver observer
                  stream.StartStreamAsync() |> ignore
                  subscription
            }
            |> Observable.map (fun e -> e.Tweet |> TwitterPost)

          // Mastodon
          let mastodonStream =
            { new IObservable<_> with
                override this.Subscribe(observer) =
                  let stream = mastodonClient.GetPublicStreaming()
                  let subscription = stream.OnUpdate |> Observable.subscribeObserver observer
                  async { stream.Start() } |> Async.Start
                  subscription
            }
            |> Observable.filteri (fun i _ -> i % 5 = 0)
            |> Observable.map (fun e -> e.Status |> MastodonPost)

          let stream =
            twitterStream |> Observable.merge mastodonStream
          return new StreamView(stream)
        | _ ->
          return! failwith "error"
      }
    new MainView(database, authenticator, createStreamView)
