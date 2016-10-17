namespace Zelga.Core

open System.Collections.ObjectModel

type TodoListSelectorViewModel =
  {
    TodoLists                   : ObservableCollection<TodoList>
  }
