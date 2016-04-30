namespace Dyxi.Muse.ViewModel

open System
open System.Windows
open System.Windows.Controls
open Dyxi.Util
open Dyxi.Muse.Model

type SettingsDialog() =
  inherit Wpf.ViewModel.DialogBase<unit>()

  let mutable path = ""

  let (uploadMusicFileCommand, _) =
    Command.create
      (konst true)
      (fun _ ->
        Media.uploadAudio path None (DateTime.Now |> Some) |> ignore
        Database.update ()
        )

  member this.Path
    with get () = path
    and  set v =
      path <- v
      this.RaisePropertyChanged("Path")

  member this.UploadMusicFileCommand = uploadMusicFileCommand
