namespace Dyxi.Util

open System

module Enum =
  let getValues<'t> =
    Enum.GetValues(typeof<'t>) |> Seq.cast<'t> |> Array.ofSeq

module Reflection =
  open Microsoft.FSharp.Reflection

  type DU<'t when 't: comparison>() =
    static member val CaseInfos =
      FSharpType.GetUnionCases(typeof<'t>)
      |> Array.toList

    static member val Names: list<string> =
      DU<'t>.CaseInfos
      |> List.map (fun (case: UnionCaseInfo) -> case.Name)

    static member TryParse(str: string): option<'t> =
      DU<'t>.CaseInfos
      |> List.tryFind (fun case -> case.Name = str)
      |> Option.map (fun case -> FSharpValue.MakeUnion (case, [||]) :?> 't)

    static member val internal StringizeUnitCaseMap: Map<'t, string> =
      [ for ci in DU<'t>.CaseInfos do
          if ci.GetFields().Length = 0 then
            yield (FSharpValue.MakeUnion(ci, Array.empty) :?> 't, ci.Name)
      ] |> Map.ofList

    static member StringizeUnitCase(case: 't): string =
      DU<'t>.StringizeUnitCaseMap |> Map.find case

    static member val UnitCases: list<'t> =
      DU<'t>.StringizeUnitCaseMap |> Map.toList |> List.map fst
