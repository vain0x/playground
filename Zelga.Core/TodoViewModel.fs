namespace Zelga.Core

open System
open System.Collections.ObjectModel
open System.Reactive.Linq
open Reactive.Bindings
open Zelga.Core.Utility

type NewReplyViewModel =
  {
    Text                        : ReactiveProperty<string>
    State                       : ReactiveProperty<option<TodoState>>
    ReplyCommand                : ReactiveCommand
  }
with
  member this.ToComment(todo: Todo, user: User) =
    let state = this.State.Value |> Option.getOr todo.CurrentState.Value
    Comment.Create(this.Text.Value, state, user)
  static member Create(todo: Todo, loginUser) =
    let text =
      ReactiveProperty.Create ""
    let command =
      new ReactiveCommand(text.Select(String.IsNullOrWhiteSpace >> not), false)
    let newCommentVm =
      {
        Text                    = text
        State                   = ReactiveProperty.Create None
        ReplyCommand            = command
      }
    let subscription =
      command
      |> Observable.subscribe
        (fun user ->
          todo.Replies.Add(newCommentVm.ToComment(todo, loginUser))
          text.Value <- ""
        )
    newCommentVm

type TodoViewModel =
  {
    Todo                        : Todo
    NewReply                    : NewReplyViewModel
    ExpanderHeader              : ReactiveProperty<string>
  }
with
  static member Create(todo: Todo, loginUser) =
    {
      Todo                      = todo
      NewReply                  = NewReplyViewModel.Create(todo, loginUser)
      ExpanderHeader            = todo.ReplyCount.Select(sprintf "コメント (%d)").ToReactiveProperty()
    }

type TodoListViewModel =
  {
    TodoList                    : TodoList
    Todos                       : ObservableCollection<TodoViewModel>
  }
with
  static member Create(todoList: TodoList, loginUser: User) =
    {
      TodoList                  = todoList
      Todos                     =
        todoList.Todos
        |> Seq.map (fun todo -> TodoViewModel.Create(todo, loginUser))
        |> ObservableCollection.OfSeq
    }

type UpdateViewModel =
  {
    User                        : User
    Update                      : Update
    Description                 : ReactiveProperty<string>
  }
with
  static member Create(user: User, update: Update, repository: Repository) =
    let description =
      match update.Action with
      | CreateTodoList todoList ->
        todoList.Name.Select(sprintf "TODOリスト「%s」を作成しました。").ToReactiveProperty()
      | CreateTodo (todoListId, todo) ->
        sprintf "TODO「%s」を作成しました。" todo.FirstComment.ShortText
        |> ReactiveProperty.Create
      | CreateReply (todoId, comment) ->
        let todo = repository.FindTodoById(todoId)
        sprintf "TODO「%s」にコメントを付けました: %s"
          todo.FirstComment.ShortText
          comment.ShortText
        |> ReactiveProperty.Create
    {
      User                      = user
      Update                    = update
      Description               = description
    }

type RepositoryViewModel =
  {
    Repository                  : Repository
    TodoList                    : ReactiveProperty<TodoListViewModel>
    TotalActivity               : Timeline<UpdateViewModel>
    SelectCommand               : ReactiveCommand<Guid>
  }
with
  static member Create(repository: Repository) =
    let loginUser = repository.Admin
    let selectCommand = new ReactiveCommand<Guid>()
    {
      Repository                = repository
      TodoList                  = ReactiveProperty.Create(TodoListViewModel.Create(repository.TodoLists.[0], loginUser))
      TotalActivity             = Timeline<UpdateViewModel>(fun updateVm -> updateVm.Update.DateTime)
      SelectCommand             = selectCommand
    }
    |> tap
      (fun this ->
        selectCommand
        |> Observable.subscribe
          (fun todoListId ->
            let todoList = repository.FindTodoListById(todoListId)
            this.TodoList.Value <- TodoListViewModel.Create(todoList, loginUser)
          ) |> ignore

        // Merge all activities.
        let mergedActivity =
          (repository.UserActivities |> ObservableCollection.ObserveAdded).SelectMany(fun activity ->
            (activity.Updates |> ObservableCollection.ObserveAdded).Select(fun update ->
              (activity.User, update)
            )
          )
        mergedActivity |> Observable.subscribe (fun (user, update) ->
          this.TotalActivity.Add(UpdateViewModel.Create(user, update, repository))
          ) |> ignore
      )
