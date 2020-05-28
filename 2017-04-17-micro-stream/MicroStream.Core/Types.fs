namespace MicroStream
  open System

  type IAuthenticator =
    abstract AuthenticateAsync: Uri -> Async<option<string>>

  type Server =
    | TwitterServer
    | MastodonServer
      of string

  type Account =
    | TwitterUser
      of Tweetinvi.Models.IUser
    | MastodonUser
      of Mastonet.Entities.Account
   with
    member this.DisplayName =
      match this with
      | TwitterUser user ->
        user.ScreenName
      | MastodonUser user ->
        user.DisplayName

     /// The name to which an `@` leads.
     member this.UserName =
      match this with
      | TwitterUser user ->
        user.Name
      | MastodonUser user ->
        user.AccountName

  type Post =
    | TwitterPost
      of Tweetinvi.Models.ITweet
    | MastodonPost
      of Mastonet.Entities.Status
   with
    member this.Content =
      match this with
      | TwitterPost post ->
        post.Text
      | MastodonPost post ->
        post.Content

    member this.Publisher =
      match this with
      | TwitterPost post ->
        post.CreatedBy |> TwitterUser
      | MastodonPost post ->
        post.Account |> MastodonUser

    member this.CreatedAt =
      match this with
      | TwitterPost post ->
        post.CreatedAt
      | MastodonPost post ->
        post.CreatedAt

namespace MicroStream.Sources
  type TwitterClient = Tweetinvi.Models.ITwitterCredentials
