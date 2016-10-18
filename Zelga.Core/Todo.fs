namespace Zelga.Core

open System
open Reactive.Bindings
open System.Collections.ObjectModel
open Zelga.Core.Utility

type User =
  Git.User

type TodoState =
  | Open
  | Closed

type Comment =
  {
    Id                          : Guid
    Text                        : string
    State                       : TodoState
    User                        : User
    Created                     : DateTime
  }
with
  static member Create(text, state, user) =
    {
      Id                        = Guid.NewGuid()
      Text                      = text
      State                     = state
      User                      = user
      Created                   = DateTime.Now
    }

type Todo =
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
      FirstComment              = Comment.Create(description, Open, user)
      Replies                   = replies
      ReplyCount                = replies |> ObservableCollection.ObserveCount
      Tags                      = ObservableCollection.Empty()
      Order                     = ReactiveProperty.Create 0.0
      CurrentState              = ReactiveProperty.Create TodoState.Open
    }

type TodoList =
  {
    Id                          : Guid
    Name                        : ReactiveProperty<string>
    Todos                       : ObservableCollection<Todo>
  }
with
  static member Create(name, todos) =
    {
      Id                        = Guid.NewGuid()
      Name                      = ReactiveProperty.Create(name)
      Todos                     = todos |> ObservableCollection.OfSeq
    }

type Repository =
  {
    TodoLists                   : ObservableCollection<TodoList>
  }

type Update =
  | CreateUser
    of User
  | CreateTodoList
    of TodoList
  | CreateTodo
    of Todo
  | CreateReply
    of todoId: Guid * Comment

[<AbstractClass>]
type UpdateCollection(user: User) =
  abstract member Add: Update -> unit
  abstract member Save: unit -> unit
  abstract member Load: unit -> Repository
