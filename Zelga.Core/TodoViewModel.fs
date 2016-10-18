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
