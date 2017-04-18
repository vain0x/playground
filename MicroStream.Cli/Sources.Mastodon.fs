namespace MicroStream.Sources

open System
open System.Data.Entity
open System.Linq
open Mastonet
open Mastonet.Entities
open MicroStream.Data.Entity

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

  let private authorizeAsync app instance userName tryCodeAsync =
    async {
      let client = MastodonClient(app)
      let! code = client.OAuthUrl() |> tryCodeAsync
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
    (database: IDatabase) (app: AppRegistration) (instance: string) (userName: string)
    (tryCodeAsync: string -> Async<Option<string>>)
    =
    async {
      let! accessToken = tryGetAccessTokenAsync database instance userName
      match accessToken with
      | Some accessToken ->
        return Some accessToken
      | None ->
        let! accessToken = authorizeAsync app instance userName tryCodeAsync
        match accessToken with
        | Some accessToken ->
          do! saveAccessTokenAsync database instance userName accessToken
          return Some accessToken
        | None ->
          return None
    }

  let tryClientAsync
    (database: IDatabase) (instance: string) (userName: string)
    (tryCodeAsync: string -> Async<Option<string>>)
    =
    async {
      let! app = getOrCreateAppAsync database instance
      let! accessToken = getOrCreateAccessTokenAsync database app instance userName tryCodeAsync
      match accessToken with
      | Some accessToken ->
        return MastodonClient(app, accessToken) |> Some
      | None ->
        return None
    }
