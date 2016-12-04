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
