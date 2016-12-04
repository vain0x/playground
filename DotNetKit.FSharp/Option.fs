namespace DotNetKit.FSharp

[<RequireQualifiedAccess>]
module Option =
  let getOr (x: 'x): option<'x> -> 'x =
    function
    | Some x -> x
    | None -> x

  let getOrElse (getX: unit -> 'x): option<'x> -> 'x =
    function
    | Some x -> x
    | None -> getX ()

  let tryApply (f: 'x -> 'y) (x: 'x): option<'y> =
    try
      f x |> Some
    with
    | _ ->
      None

  let tryCatch (f: 'x -> unit) (x: 'x): option<'e> =
    try
      f x
      None
    with
    | :? 'e as e ->
      Some e

  let ofTry: bool * 'x -> option<'x> =
    function
    | (true, x) ->
      Some x
    | (false, _) ->
      None

namespace DotNetKit.FSharp.ComputationExpression
  [<Sealed>]
  type OptionBuilder internal () =
    member this.Zero() =
      None

    member this.Return(x) =
      Some x

    member this.ReturnFrom(option: option<_>) =
      option

    member this.Bind(m, f) =
      m |> Option.bind f

    member this.Using(x, f) =
      using x f

namespace DotNetKit.FSharp
  open DotNetKit.FSharp.ComputationExpression

  [<AutoOpen>]
  module OptionSyntax =
    let option = OptionBuilder()
