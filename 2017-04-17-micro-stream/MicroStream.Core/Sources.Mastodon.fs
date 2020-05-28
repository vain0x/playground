namespace MicroStream.Sources

open System
open System.Data.Entity
open System.Linq
open FSharp.Control.Reactive
open Mastonet
open Mastonet.Entities
open MicroStream
open MicroStream.Data.Entity

exception MastodonLoginFailure
  of instance: string * userName: string

module Mastodon =
  [<Literal>]
  let appName = "MicroStream"

  [<Literal>]
  let appUrl = "http://github.com/vain0/MicroStream"

  let private createAppAsync (instance: string) =
    async {
      let scope = Scope.Read ||| Scope.Write ||| Scope.Follow
      return! MastodonClient.CreateApp(instance, appName, scope, appUrl) |> Async.AwaitTask
    }

  let private saveAppAsync database (context: DbContext) (app: AppRegistration) =
    async {
      let! id = database |> Database.generateIdAsync
      let mastodonApp = MastodonApp.Create(id, app.Instance, app.ClientId, app.ClientSecret)
      context.Set<MastodonApp>().Add(mastodonApp) |> ignore
      return! context |> DbContext.saveAsync
    }

  let private getOrCreateAppAsync database (instance: string) =
    async {
      let context = database |> Database.connect
      let! app = context.Set<MastodonApp>().TryFirstAsync(fun app -> app.Instance = instance)
      match app with
      | Some app ->
        return
          AppRegistration
            ( Instance = app.Instance
            , ClientId = app.ClientId
            , ClientSecret = app.ClientSecret
            )
      | None ->
        let! app = createAppAsync instance
        do! saveAppAsync database context app
        return app
    }

  let private tryGetAccessTokenAsync database instance userName =
    async {
      let context = database |> Database.connect
      let! auth =
        context.Set<MastodonAuth>()
          .TryFirstAsync(fun auth -> auth.Instance = instance && auth.UserName = userName)
      match auth with
      | Some auth ->
        return auth.AccessToken |> Some
      | None ->
        return None
    }

  let private authorizeAsync authenticator app instance userName =
    async {
      let client = MastodonClient(app)
      let! code = (authenticator: IAuthenticator).AuthenticateAsync(client.OAuthUrl() |> Uri)
      match code with
      | Some code ->
        let! auth = client.ConnectWithCode(code) |> Async.AwaitTask
        return Some auth.AccessToken
      | None ->
        return None
    }

  let private saveAccessTokenAsync database instance userName accessToken =
    async {
      let context = database |> Database.connect
      let! id = database |> Database.generateIdAsync
      let auth = MastodonAuth.Create(id, instance, userName, accessToken)
      context.Set<MastodonAuth>().Add(auth) |> ignore
      return! context |> DbContext.saveAsync
    }

  let private getOrCreateAccessTokenAsync
    (database: IDatabase) authenticator (app: AppRegistration) (instance: string) (userName: string)
    =
    async {
      let! accessToken = tryGetAccessTokenAsync database instance userName
      match accessToken with
      | Some accessToken ->
        return Some accessToken
      | None ->
        let! accessToken = authorizeAsync authenticator app instance userName
        match accessToken with
        | Some accessToken ->
          do! saveAccessTokenAsync database instance userName accessToken
          return Some accessToken
        | None ->
          return None
    }

  let tryClientAsync
    (database: IDatabase) (authenticator: IAuthenticator) (instance: string) (userName: string)
    =
    async {
      let! app = getOrCreateAppAsync database instance
      let! accessToken = getOrCreateAccessTokenAsync database authenticator app instance userName
      match accessToken with
      | Some accessToken ->
        return MastodonClient(app, accessToken) |> Some
      | None ->
        return None
    }

  let publicStreamAsync
    (database: IDatabase)
    (authenticator: IAuthenticator)
    (instance: string)
    (userName: string)
    =
    async {
      let! client = tryClientAsync database authenticator instance userName
      match client with
      | Some client ->
        return
          { new IObservable<_> with
              override this.Subscribe(observer) =
                let stream = client.GetPublicStreaming()
                let subscription = stream.OnUpdate |> Observable.subscribeObserver observer
                stream.Start()
                subscription
          }
          |> Observable.filteri (fun i _ -> i % 5 = 0)
          |> Observable.map (fun e -> e.Status |> MastodonPost)
      | None ->
        return! MastodonLoginFailure (instance, userName) |> raise
    }
