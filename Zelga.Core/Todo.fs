namespace Zelga.Core

open System
open Reactive.Bindings
open System.Collections.ObjectModel
open Zelga.Core.Utility

type TodoState =
  | Open
  | Closed

type Comment =
  {
    Text                        : string
    State                       : TodoState
    User                        : User
    Created                     : DateTime
  }
with
  static member Create(text, user) =
    {
      Text                      = text
      State                     = Open
      User                      = user
      Created                   = DateTime.Now
    }

type Todo =
  {
    FirstComment                : Comment
    Comments                    : ObservableCollection<Comment>
    Tags                        : ObservableCollection<string>
    Order                       : ReactiveProperty<double>
  }
with
  static member Create(description, user) =
    {
      FirstComment              = Comment.Create(description, user)
      Comments                  = ObservableCollection.Empty
      Tags                      = ObservableCollection.Empty
      Order                     = ReactiveProperty.Create 0.0
    }
