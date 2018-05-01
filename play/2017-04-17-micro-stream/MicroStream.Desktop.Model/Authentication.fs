namespace MicroStream.Authentication

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

/// An authenticator easy to use in C#.
[<AbstractClass>]
type CSharpAuthenticator() =
  abstract TryAuthenticateAsync: Uri -> Task<bool * string>

  member this.AuthenticateAsync(uri: Uri) =
    async {
      let! (hasValue, code) = this.TryAuthenticateAsync uri |> Async.AwaitTask
      return if hasValue then Some code else None
    }

  interface IAuthenticator with
    override this.AuthenticateAsync(uri: Uri) =
      this.AuthenticateAsync(uri)
