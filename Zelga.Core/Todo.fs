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

and Todo =
  {
    FirstComment                : Comment
    Replies                     : ObservableCollection<Comment>
    ReplyCount                  : ReactiveProperty<int>
    Tags                        : ObservableCollection<string>
    Order                       : ReactiveProperty<double>
    CurrentState                : ReactiveProperty<TodoState>
  }
with
  static member Create(description, user) =
    let replies = ObservableCollection.Empty()
    {
      FirstComment              = Comment.Create(description, user)
      Replies                   = replies
      ReplyCount                = replies |> ObservableCollection.ObserveCount
      Tags                      = ObservableCollection.Empty()
      Order                     = ReactiveProperty.Create 0.0
      CurrentState              = ReactiveProperty.Create TodoState.Open
    }

type TodoList =
  {
    Id                          : int
    Name                        : ReactiveProperty<string>
    Todos                       : ObservableCollection<Todo>
  }
with
  static member Create(name, todos) =
    {
      Id                        = Id.Generate ()
      Name                      = ReactiveProperty.Create(name)
      Todos                     = todos |> ObservableCollection.OfSeq
    }

type Repository =
  {
    TodoLists                   : ObservableCollection<TodoList>
  }
