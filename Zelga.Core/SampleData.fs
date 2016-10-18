namespace Zelga.Core

open Zelga.Core.Utility

module SampleData =
  let admin = User.Create("admin", "admin@example.com")
  let miku = User.Create("Miku", "miku@kr.com")
  let yukari = User.Create("Yukari", "yukari@makets.com")

  let repository = Repository.Empty(admin)

  let repositoryVm =
    RepositoryViewModel.Create(repository)

  let createTodo (todoList: TodoList) name (activity: Activity) =
    Todo.Create(name, activity.User)
    |> tap
      (fun todo ->
        activity.Add(CreateTodo (todoList.Id, todo))
      )

  let createReply (todo: Todo) state message (activity: Activity) =
    activity.Add(CreateReply (todo.Id, Comment.Create(message, state, activity.User)))

  let mikuActivity =
    repository.AddUser(miku)

  let yukariActivity =
    repository.AddUser(yukari)

  let mikuTodoList = TodoList.Empty("miku's")

  mikuActivity.Add(CreateTodoList mikuTodoList)

  let todo1 =
    mikuActivity |> createTodo mikuTodoList "The first todo created by miku."

  mikuActivity |> createReply todo1 Open "The first comment."
  yukariActivity |> createReply todo1 Open "The second comment."

  let todo2 =
    yukariActivity |> createTodo mikuTodoList "The second todo created by yukari."

  let todo3 =
    yukariActivity |> createTodo mikuTodoList "A closed todo."

  mikuActivity |> createReply todo3 Closed "Closes it."
