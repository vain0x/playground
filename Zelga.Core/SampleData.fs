namespace Zelga.Core

open Zelga.Core.Utility

module SampleData =
  let admin = User.Create("admin", "admin@example.com")
  let vain = User.Create("vain", "vain@example.com")
  let uedai = User.Create("uedai", "uedai@example.com")

  let repository = Repository.Empty(admin)

  repository.AdminTimeLine.Updates.Add(CreateUser(vain))
  repository.AdminTimeLine.Updates.Add(CreateUser(uedai))

  let users =
    [
      vain
      uedai
    ]

  let todo1 =
    let it = Todo.Create("The first todo created by vain.", vain)
    it.Replies.Add(Comment.Create("Hey this is the first comment!", Open, uedai))
    it.Replies.Add(Comment.Create("3個目のコメント。", Open, vain))
    it

  let todo2 =
    Todo.Create("This is the second todo, which has no comments.", uedai)

  let todo3 =
    Todo.Create("This is a closed todo", vain)
    |> tap
      (fun todo ->
        todo.Replies.Add(Comment.Create("It is done.", Closed, vain))
      )

  let todos =
    [
      todo1
      todo2
      todo3
    ]

  let todoList =
    TodoList.Create("The todo list", todos)

  let todoListVm =
    TodoListViewModel.Create(todoList, admin)

  let repository =
    {
      TodoLists =
        [
          todoList
        ] |> ObservableCollection.OfSeq
    }

  let adminUpdates =
    [
      Update.CreateUser(vain)
      Update.CreateUser(uedai)
    ]

  let adminUpdateCollection =
    new UpdateCollection(admin)

  let vainTimeline =
    [
      Update ()
    ]
