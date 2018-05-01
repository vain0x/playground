namespace MicroStream.Scripting

open System
open System.Reactive.Disposables
open FSharp.Control.Reactive
open FSharpKit.ErrorHandling
open MicroStream
open MicroStream.Data.Entity
open MicroStream.Sources

type SourceStream =
  | TwitterUserStream
    of userName: string
  | MastodonPublicStream
    of instance: string * userName: string

type Stream =
  | SourceStream
    of SourceStream
  | MergedStream
    of array<Stream>

exception ScriptParseFailure of string

module Runner =
  let internal parse (source: string) =
    [|
      for line in source.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries) ->
        if line.StartsWith("#") then
          None
        else
          let stream =
            match line.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries) with
            | [|"twitter-user"; userName|] ->
              TwitterUserStream userName
            | [|"mastodon-public"; instance; userName|] ->
              MastodonPublicStream (instance, userName)
            | _ ->
              ScriptParseFailure line |> raise
          SourceStream stream |> Some
    |]
    |> Array.choose id
    |> MergedStream

  let internal evaluateAsync database authenticator stream =
    let rec loop stream =
      async {
        match stream with
        | SourceStream stream ->
          match stream with
          | TwitterUserStream userName ->
            return! Twitter.userStreamAsync database authenticator userName
          | MastodonPublicStream (instance, userName) ->
            return! Mastodon.publicStreamAsync database authenticator instance userName
        | MergedStream streams ->
          match streams with
          | [||] ->
            return Observable.empty
          | [|stream|] ->
            return! stream |> loop
          | streams ->
            let! streams = streams |> Array.map loop |> Async.Parallel
            return streams |> Observable.mergeArray
      }
    loop stream

  let internal messageFromEvaluationException e =
    match e with
    | TwitterLoginFailure userName ->
      sprintf "Couldn't login to Twitter as @%s." userName
    | MastodonLoginFailure (instance, userName) ->
      sprintf "Couldn't login to Mastodon as @%s@%s." instance userName
    | e ->
      e |> string

  let tryRunAsync (database: IDatabase) (authenticator: IAuthenticator) source =
    async {
      try
        let! observable = source |> parse |> evaluateAsync database authenticator
        return Ok observable
      with
      | e ->
        return e |> messageFromEvaluationException |> Error
    }
