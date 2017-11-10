module Dyxi.Util.Wpf

open System
open System.Collections.ObjectModel
open System.ComponentModel
open System.Windows
open System.Windows.Input

module Seq =
  let toObservableCollection (xs: seq<_>) =
    ObservableCollection(xs)

module private NotifyPropertyChanged =
  let create sender =
    let ev              = Event<_, _>()
    let trigger name    = ev.Trigger(sender, PropertyChangedEventArgs(name))
    in (ev.Publish, trigger)

module Command =
  let create (canExecute: obj -> bool) (execute: obj -> unit) =
    let canExecuteChanged =
      Event<EventHandler, EventArgs>()
    let triggerCanExecuteChanged sender =
      canExecuteChanged.Trigger(sender, null)
    let command =
      { new ICommand with
          member this.CanExecute(param) = canExecute param
          member this.Execute(param) = execute param
          [<CLIEvent>]
          member this.CanExecuteChanged = canExecuteChanged.Publish
      }
    in (command, triggerCanExecuteChanged)

module ViewModel =
  type Base() as this =
    let (propertyChanged, raisePropertyChanged) =
      NotifyPropertyChanged.create this

    member this.RaisePropertyChanged(propertyName) =
      propertyName |> raisePropertyChanged

    interface INotifyPropertyChanged with
      [<CLIEvent>]
      member this.PropertyChanged = propertyChanged
