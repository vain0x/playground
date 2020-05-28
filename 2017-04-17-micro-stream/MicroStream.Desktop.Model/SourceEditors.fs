namespace MicroStream.SourceEditors

open System
open System.Reactive.Disposables
open System.Reactive.Subjects
open FSharp.Control.Reactive
open MicroStream
open MicroStream.Reactive.Bindings
open MicroStream.Scripting
open global.Reactive.Bindings

type SourceEditor(run) =
  let source = Behavior.create ""

  let error = Behavior.create ""

  let loadCommand = new ReactiveCommand<unit>()

  let runCommand =
    let canExecute = source |> Observable.map (fun s -> String.IsNullOrWhiteSpace(s) |> not)
    new ReactiveCommand<unit>(canExecute)

  let updated = new Subject<IObservable<Post>>()

  let load () =
    try
      source.Value <-
        System.IO.File.ReadAllText("./data/default.micro-stream-script")
    with
    | e ->
      error.Value <- e |> string

  do loadCommand |> Observable.subscribe load |> ignore

  let run () =
    async {
      let source = source.Value
      if String.IsNullOrWhiteSpace(source) |> not then
        let! observable = source |> run
        match observable with
        | Ok observable ->
          error.Value <- "Success."
          updated.OnNext(observable)
        | Error message ->
          error.Value <- message
    } |> Async.Start

  do runCommand |> Observable.subscribe run |> ignore

  member this.Source = source
  member this.Error = error

  member this.LoadCommand = loadCommand
  member this.RunCommand = runCommand

  member this.Updated =
    updated :> IObservable<_>

  member this.Start() =
    load ()
    run ()
