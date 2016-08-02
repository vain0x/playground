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

[<AutoOpen>]
module Constants =
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
      // F#
      ".fsproj"
      // C#
      ".csproj"
      // Visual Basic
      ".vb"
      ".vbproj"
      ".myapp"
    ]
    |> set

  let guids =
    [
      // F#
      "d0a951bb-4d6b-4d6e-8232-cdf882198bcd"
      "2611FE50-25CF-403B-B433-1D15714837C2"
      // C#
      "F2CD3249-5799-4B50-AB79-C957C36930BE"
      "C2FB86B2-62B1-469C-9B53-4A9563597BCE"
      "f2cd3249-5799-4b50-ab79-c957c36930be"
      "c2fb86b2-62b1-469c-9b53-4a9563597bce"
      // Visual Basic
      "F184B08F-C81C-45F6-A57F-5ABD9991F28F"
      "5F1923B2-C8BB-4A0E-9F7F-25D9DDBB1A92"
      "36757C03-1694-486D-B93B-E8FE94979DCA"
      "8a823414-0fc0-4d53-a0de-2403f7bc0740"
      "5c50d068-e747-4444-8114-955d070e827c"
    ]
