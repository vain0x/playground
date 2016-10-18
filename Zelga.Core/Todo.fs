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
    Created                     : DateTimeOffset
  }
with
  static member Create(text, state, user) =
    {
      Id                        = Guid.NewGuid()
      Text                      = text
      State                     = state
      User                      = user
      Created                   = DateTimeOffset.Now
    }

type Todo =
  {
    Id                          : Guid
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
      Id                        = Guid.NewGuid()
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
  static member Empty(name) =
    {
      Id                        = Guid.NewGuid()
      Name                      = ReactiveProperty.Create(name)
      Todos                     = ObservableCollection.Empty()
    }

type Update =
  | CreateTodoList
    of TodoList
  | CreateTodo
    of todoListId: Guid * Todo
  | CreateReply
    of todoId: Guid * Comment

type Activity =
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
    UserActivities              : ObservableCollection<Activity>
    TodoLists                   : ObservableCollection<TodoList>
  }
with
  member this.Update(updater, update) =
    match update with
    | CreateTodoList todoList ->
      this.TodoLists.Add(todoList)
    | CreateTodo (todoListId, todo) ->
      this.TodoLists
      |> Seq.tryFind (fun tl -> tl.Id = todoListId)
      |> Option.iter (fun todoList -> todoList.Todos.Add(todo))
    | CreateReply (todoId, comment) ->
      this.TodoLists
      |> Seq.collect (fun tl -> tl.Todos)
      |> Seq.tryFind (fun todo -> todo.Id = todoId)
      |> Option.iter (fun todo -> todo.Replies.Add(comment))

  member this.AddUser(user) =
    let timeline = Activity.Empty(user)
    let subscription =
      timeline.Updates
      |> ObservableCollection.ObserveAdded
      |> Observable.subscribe (fun update -> this.Update(user, update))
    this.UserActivities.Add(timeline)
    
  static member Empty(admin) =
    {
      Admin                     = admin
      UserActivities            = ObservableCollection.Empty()
      TodoLists                 = ObservableCollection.Empty()
    }
    |> tap (fun this -> this.AddUser(admin))
