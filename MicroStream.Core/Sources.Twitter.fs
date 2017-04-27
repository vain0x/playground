namespace MicroStream.Sources

open System
open System.Data.Entity
open System.Data.Entity.Infrastructure
open System.Linq
open System.Reactive.Subjects
open FSharp.Control.Reactive
open FSharpKit
open MicroStream
open MicroStream.Data.Entity

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Twitter =
  [<Literal>]
  let consumerKey = "5wBxtl0U9PnKLd0I2BIekunvO"

  [<Literal>]
  let consumerSecret = "xiPREh891FZa7YaIj1sQD3vcSRuXe5fUFKM7IVfgCdBgogv98m"

  let private clientFromAccessToken accessToken accessTokenSecret =
    Tweetinvi.Auth.CreateCredentials
      ( consumerKey
      , consumerSecret
      , accessToken
      , accessTokenSecret
      )

  let private authorizeAsync (authenticator: IAuthenticator) userName =
    async {
      let twitterCredential = Tweetinvi.Models.TwitterCredentials(consumerKey, consumerSecret)
      let authContext = Tweetinvi.AuthFlow.InitAuthentication(twitterCredential)
      let! code = authenticator.AuthenticateAsync(authContext.AuthorizationURL |> Uri)
      match code with
      | Some code ->
        match Tweetinvi.AuthFlow.CreateCredentialsFromVerifierCode(code, authContext) with
        | null ->
          return! failwith "Incorrect pin code."
        | client ->
          return Some client
      | None ->
        return None
    }

  let private saveAccessTokenAsync database userName accessToken accessTokenSecret =
    async {
      use context = database |> Database.connect
      let! authId = database |> Database.generateIdAsync
      let auth = TwitterAuth.Create(authId, userName, accessToken, accessTokenSecret)
      context.Set<TwitterAuth>().Add(auth) |> ignore
      return! context |> DbContext.saveAsync
    }

  let tryClientAsync database authenticator userName =
    async {
      use context = database |> Database.connect
      let! auth = context.Set<TwitterAuth>().TryFirstAsync()
      match auth with
      | Some auth ->
        return clientFromAccessToken auth.AccessToken auth.AccessTokenSecret |> Some
      | None ->
        let! client = authorizeAsync authenticator userName
        match client with
        | Some client ->
          do! saveAccessTokenAsync database userName client.AccessToken client.AccessTokenSecret
          return Some client
        | None ->
          return None
    }
