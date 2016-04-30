namespace Dyxi.Muse.ViewModel

open Dyxi.Util
open Dyxi.Muse.Model

type MainWindow() =
  inherit Wpf.ViewModel.Base()

  let mutable coll = MusicColl

  let mutable tracks = ([||]: LvRow [])

  let mutable stack = ([||]: MediaId [])

  let settingsDialog = SettingsDialog()

  let (openSettingsCommand, _) =
    Command.create
      (konst true)
      (fun _ -> settingsDialog.Show(()))

  let updateTracks () =
    tracks <-
      [|
        for media in Coll.fetchMediaList coll do
          yield
            {
              Title       = media.name
              Album       = ""
              Composer    = ""
              Writer      = ""
              Added       = ""
              LastPlayed  = ""
              MediaId     = media.id
            }
      |]

  member this.Coll
    with get () = coll
    and  set v  =
      coll <- v
      updateTracks ()
      for name in ["Coll"; "Tracks"] do
        this.RaisePropertyChanged(name)

  member this.Tracks
    with get () = tracks

  member this.SettingsDialog = settingsDialog

  member this.OpenSettingsCommand = openSettingsCommand
