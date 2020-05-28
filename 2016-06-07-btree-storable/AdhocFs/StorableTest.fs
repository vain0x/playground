namespace AdhocFs

open System
open System.IO
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

module StorableTest =
  type Node =
    {
      Name          : string
      Children      : list<StorableAsYaml<Node>>
    }

  let storableNode node =
    let path = Path.Combine("C:/storable_test", node.Name |> string)
    let file = FileInfo(path)
    let storable = new StorableAsYaml<Node>(file)
    storable.Value <- node
    storable

  let seed () =
    storableNode
      {
        Name = "a"
        Children =
          [
            (storableNode { Name = "b"; Children = [] })
            (storableNode { Name = "c"; Children = [] })
            storableNode
              {
                Name = "d"
                Children =
                  [
                    (storableNode { Name = "e"; Children = [] })
                  ]
              }
          ]
      }

  let theTest =
    test {
      // Seed.
      let repo = DirectoryInfo("C:/storable_test")
      repo.Create()

      let testNode = seed ()
      do! testNode.IsLoaded |> assertEquals true

      // Store test.
      let rec loop (n: StorableAsYaml<Node>) =
        n.Value.Children |> List.iter loop
        n.Store()
      in loop testNode
      do! testNode.IsLoaded |> assertEquals false

      // Fetch test.
      do! testNode.Value.Name |> assertEquals "a"
      do! testNode.IsLoaded |> assertEquals true
      do!
        testNode.Value.Children
        |> List.forall (fun node -> node.IsLoaded |> not)
        |> assertPred

      // Clean up.
      let rec loop (n: StorableAsYaml<Node>) =
        n.Value.Children |> List.iter loop
        n.Clear()
      in loop testNode

      repo.Delete()
      return ()
    }
