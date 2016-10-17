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
  member this.ToComment(todo: Todo) =
    {
      Text                      = this.Text.Value
      State                     = this.State.Value |> Option.getOr todo.CurrentState.Value
      User                      = LoginInfo.Current
      Created                   = DateTime.Now
    }
  static member Create(todo: Todo) =
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
        (fun _ ->
          todo.Replies.Add(newCommentVm.ToComment(todo))
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
  static member Create(todo: Todo) =
    {
      Todo                      = todo
      NewReply                  = NewReplyViewModel.Create(todo)
      ExpanderHeader            = todo.ReplyCount.Select(sprintf "コメント (%d)").ToReactiveProperty()
    }

type TodoListViewModel =
  {
    TodoList                    : TodoList
    Todos                       : ObservableCollection<TodoViewModel>
  }
with
  static member Create(todoList: TodoList) =
    {
      TodoList                  = todoList
      Todos                     =
        todoList.Todos |> Seq.map TodoViewModel.Create |> ObservableCollection.OfSeq
    }
