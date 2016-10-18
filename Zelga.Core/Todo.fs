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

type Update =
  | CreateUser
    of User
  | CreateTodoList
    of TodoList
  | CreateTodo
    of Todo
  | CreateReply
    of todoId: Guid * Comment

type Timeline =
  {
    User                        : User
    Updates                     : ObservableCollection<Update>
  }
with
  static member Empty(user: User) =
    {
      User                      = user
      Updates                   = ObservableCollection.Empty()
    }

type Repository =
  {
    Admin                       : User
    UserTimelines               : ObservableCollection<Timeline>
    TodoLists                   : ObservableCollection<TodoList>
  }
with
  member this.Update(updater, update) =
    match update with
    | CreateUser user ->
      this.AddUser(updater, user)
    | CreateTodoList todoList ->
      ()
    | CreateTodo todo ->
      ()
    | CreateReply (todoId, comment) ->
      ()

  member this.AddUser(updater, user) =
    let timeline = Timeline.Empty(user)
    let updated = timeline.Updates |> ObservableCollection.ObserveAdded
    updated |> Observable.subscribe (fun update -> this.Update(user, update)) |> ignore
    this.UserTimelines.Add(timeline)
    
  static member Empty(admin) =
    {
      Admin                     = admin
      UserTimelines             = ObservableCollection.Empty()
      TodoLists                 = ObservableCollection.Empty()
    }
    |> tap (fun this -> this.AddUser(admin, admin))
