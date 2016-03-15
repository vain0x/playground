namespace Util

open System
open Basis.Core

[<AutoOpen>]
module Misc =
  /// constant function
  let inline konst x _ = x

  let inline flip f x y = f y x

  let inline curry2 f x0 x1    = f (x0, x1)
  let inline curry3 f x0 x1 x2 = f (x0, x1, x2)

  let inline uncurry2 f (x0, x1    ) = f x0 x1
  let inline uncurry3 f (x0, x1, x2) = f x0 x1 x2

  /// apply f to x; then return x
  let tap f x =
    do f x
    x

  /// ignore typed
  let inline ignore'<'T> (_ : 'T) = ()

  /// assert in pipeline
  let inline assert' pred =
    tap (fun x -> assert (pred x))

module Math =
  let numDigits n =
    if n = 0
    then 1
    else n |> Math.Abs |> float |> Math.Log10 |> int |> (+) 1

module Str =
  let private optFromIndex i =
    if i >= 0 then Some i else None

  let tryIndexOf     target = (Str.indexOf target) >> optFromIndex
  let tryLastIndexOf target = (Str.lastIndexOf target) >> optFromIndex

module Integer =
  let (|Positive|Zero|Negative|) i =
    if   i = 0 then Zero
    elif i > 0 then Positive i
    else Negative i

[<AutoOpen>]
module RegexExtension =
  open System.Text.RegularExpressions
  open System.Linq

  let (|Matches|) pattern input =
    let m = Regex.Matches(input, pattern)
    m.Cast<Match>()

  type Group with
    member this.TryValue =
      if this.Success then Some (this.Value) else None

module Encoding =
  let Shift_JIS = Text.Encoding.GetEncoding("Shift_JIS")

module Path =
  open System.IO

  let Escape escaper path =
    let rec esc path c =
        path |> Str.replace (string c) (escaper c)

    Path.GetInvalidFileNameChars()
    |> Array.fold esc path

module Reflection =
  open Microsoft.FSharp.Reflection

  type DU<'T> () =
    static member val CaseInfos =
        FSharpType.GetUnionCases(typeof<'T>)
        |> Array.toList

    static member val Names =
        DU<'T>.CaseInfos
        |> List.map (fun (case: UnionCaseInfo) -> case.Name)

    static member val UnitCases =
        DU<'T>.CaseInfos
        |> List.choose (fun ci ->
            if ci.GetFields().Length = 0
            then Some (FSharpValue.MakeUnion(ci, Array.empty) :?> 'T)
            else None
          )

    static member FromString str =
        let caseOpt =
            DU<'T>.CaseInfos
            |> List.tryFind (fun case -> case.Name = str)
        match caseOpt with
        | Some case -> FSharpValue.MakeUnion (case, [||])
        | None -> failwith ("unknown case of " + typeof<'T>.Name)
