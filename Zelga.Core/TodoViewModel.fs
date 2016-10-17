namespace Zelga.Core

open System.Collections.ObjectModel
open Reactive.Bindings
open Zelga.Core.Utility

type CommentViewModel =
  {
    Comment             : Comment
  }
with
  static member Create(comment) =
    {
      Comment           = comment
    }

type TodoViewModel =
  {
    Todo                : Todo
    FirstComment        : CommentViewModel
    Comments            : ObservableCollection<CommentViewModel>
    ExpanderHeader      : ReactiveProperty<string>
  }
with
  static member Create(todo: Todo) =
    {
      Todo              = todo
      FirstComment      =
        todo.FirstComment |> CommentViewModel.Create
      Comments          =
        todo.Comments |> Seq.map CommentViewModel.Create |> ObservableCollection.OfSeq
      ExpanderHeader    = ReactiveProperty.Create "コメント (0)"
    }

type TodoListViewModel =
  {
    Name                : ReactiveProperty<string>
    Todos               : ObservableCollection<TodoViewModel>
  }
with
  static member Create(todoList: TodoList) =
    {
      Name              = todoList.Name
      Todos             = todoList.Todos |> Seq.map TodoViewModel.Create |> ObservableCollection.OfSeq
    }
