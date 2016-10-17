namespace Zelga.Core

open Zelga.Core.Utility

module SampleData =
  let vain = User.Create("vain", "vain@example.com")
  let uedai = User.Create("uedai", "uedai@example.com")

  let users =
    [
      vain
      uedai
    ]

  let todo1 =
    let it = Todo.Create("The first todo created by vain.", vain)
    it.Replies.Add(Comment.Create("Hey this is the first comment!", uedai))
    it.Replies.Add(Comment.Create("3個目のコメント。", vain))
    it

  let todo2 =
    Todo.Create("This is the second todo, which has no comments.", uedai)

  let todo3 =
    Todo.Create("This is the third todo", vain)

  let todos =
    [
      todo1
      todo2
      todo3
    ]

  let todoList =
    TodoList.Create("The todo list", todos)

  let todoListVm =
    TodoListViewModel.Create(todoList)

  let repository =
    {
      TodoLists =
        [
          todoList
        ] |> ObservableCollection.OfSeq
    }
