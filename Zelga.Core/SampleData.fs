namespace Zelga.Core

open Zelga.Core.Utility

module SampleData =
  let admin = User.Create("admin", "admin@example.com")
  let miku = User.Create("Miku", "miku@kr.com")
  let yukari = User.Create("Yukari", "yukari@makets.com")

  let repository = Repository.Empty(admin)

  let createTodo (todoList: TodoList) name updater =
    let todo = Todo.Create(name, updater)
    repository.Update(updater, CreateTodo (todoList.Id, todo))
    todo

  let createReply (todo: Todo) state message updater =
    repository.Update(updater, CreateReply (todo.Id, Comment.Create(message, state, updater)))

  repository.AddUser(miku)
  repository.AddUser(yukari)

  let mikuTodoList = TodoList.Empty("miku's")

  repository.Update(miku, CreateTodoList mikuTodoList)

  let todo1 =
    miku |> createTodo mikuTodoList "The first todo created by miku."

  miku |> createReply todo1 Open "The first comment."
  yukari |> createReply todo1 Open "The second comment."

  let todo2 =
    yukari |> createTodo mikuTodoList "The second todo created by yukari."

  let todo3 =
    yukari |> createTodo mikuTodoList "A closed todo."

  miku |> createReply todo3 Closed "Closes it."

  let repositoryVm =
    RepositoryViewModel.Create(repository)
