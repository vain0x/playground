namespace SolutionRenamer

[<AutoOpen>]
module Types =

  open System

  type RenamerState =
    {
      Map                 : Map<string, string>
      Extensions          : Set<string>
      IgnoreList          : Set<string>
    }

  type GuidCollectorState =
    {
      Renamer             : RenamerState
      Guids               : Set<string>
    }

module Constants =
  open System.Text.RegularExpressions

  let ignoreList =
    [
      "rename_solution.bat"
      "SolutionRenamer.exe"
      "solution_renamer"
    ]
    |> set

  let extensions =
    [
      ".sln"
      ".txt"
      ".md"
      ".html"
      ".xml"
      ".bat"
      ".sh"
      // F#
      ".fs"
      ".fsi"
      ".fsx"
      ".fsproj"
      // C#
      ".cs"
      ".csproj"
      // Visual Basic
      ".vb"
      ".vbproj"
      ".myapp"
    ]
    |> set

  let guidRegex =
    new Regex(@"\b[a-fA-F0-9]{8}(?:-[a-fA-F0-9]{4}){3}-[a-fA-F0-9]{12}\b")
