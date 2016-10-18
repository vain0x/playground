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
      Created                   = DateTimeOffset.UtcNow
    }
  member this.ShortText =
    this.Text |> String.shorten

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

type UpdateAction =
  | CreateTodoList
    of TodoList
  | CreateTodo
    of todoListId: Guid * Todo
  | CreateReply
    of todoId: Guid * Comment

type Update =
  {
    Action                      : UpdateAction
    DateTime                    : DateTimeOffset
  }
with
  static member Create(action) =
    {
      Action                    = action
      DateTime                  = DateTimeOffset.UtcNow
    }

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
  member this.Add(updateAction) =
    this.Updates.Add(Update.Create(updateAction))

type Repository =
  {
    Admin                       : User
    UserActivities              : ObservableCollection<Activity>
    TodoLists                   : ObservableCollection<TodoList>
  }
with
  member this.FindTodoListById(todoListId) =
    this.TodoLists |> Seq.find (fun tl -> tl.Id = todoListId)

  member this.AllTodo =
    this.TodoLists |> Seq.collect (fun tl -> tl.Todos)

  member this.FindTodoById(todoId) =
    this.AllTodo |> Seq.find (fun todo -> todo.Id = todoId)

  member this.Update(updater, update: Update) =
    match update.Action with
    | CreateTodoList todoList ->
      this.TodoLists.Add(todoList)
    | CreateTodo (todoListId, todo) ->
      let todoList = this.FindTodoListById(todoListId)
      todoList.Todos.Add(todo)
    | CreateReply (todoId, comment) ->
      let todo = this.FindTodoById(todoId)
      todo.Replies.Add(comment)

  member this.AddUser(user) =
    Activity.Empty(user)
    |> tap
      (fun activity ->
        let subscription =
          activity.Updates
          |> ObservableCollection.ObserveAdded
          |> Observable.subscribe (fun update -> this.Update(user, update))
        this.UserActivities.Add(activity)
      )
    
  static member Empty(admin) =
    {
      Admin                     = admin
      UserActivities            = ObservableCollection.Empty()
      TodoLists                 = ObservableCollection.Empty()
    }
    |> tap (fun this ->
      this.TodoLists.Add(TodoList.Empty("Default"))
    )
