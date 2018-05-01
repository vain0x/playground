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
open MicroStream.Scripting
open MicroStream.Sources
open MicroStream.SourceEditors
open MicroStream.StreamViews
open global.Reactive.Bindings

type MainView
  ( database: IDatabase
  , authenticator: IAuthenticator
  , sourceEditor: SourceEditor
  , streamView: StreamView
  ) =
  member this.Authenticator = authenticator

  member this.SourceEditor = sourceEditor

  member this.StreamView = streamView

  member this.Start() = sourceEditor.Start()

  member this.Dispose() =
    this.StreamView.Dispose()

  interface IDisposable with
    override this.Dispose() = this.Dispose()

  static member Create(authenticator: IAuthenticator) =
    let database = AppDatabase()
    let run = Runner.tryRunAsync database authenticator
    let sourceEditor = SourceEditor(run)
    let streamView = new StreamView(sourceEditor.Updated |> Observable.switch)
    new MainView(database, authenticator, sourceEditor, streamView)
