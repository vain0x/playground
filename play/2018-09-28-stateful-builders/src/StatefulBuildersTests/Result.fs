namespace StatefulBuilders

module Internals =
  let inline combine r f =
    match r with
    | Ok x -> f x
    | Error e -> Error e

type ResultMinimalBuilder internal () =
  member inline __.Return(x) =
    Ok x

  member inline __.ReturnFrom(result: Result<_, _>) =
    result

  member inline __.Zero() =
    Ok ()

  member inline __.Bind(m, f) =
    m |> Result.bind f

  member inline __.Using(x, f) =
    use x = x
    f x

type ResultFullBuilder internal () =
  inherit ResultMinimalBuilder()

  member inline __.Run(f): Result<'x, 'e> = f ()

  member inline __.Delay(f): unit -> Result<'x, 'e> = f

  member inline __.TryWith(f, h): Result<'x, 'e> =
    try
      f ()
    with
    | e -> h e

  member inline __.TryFinally(f, g): Result<'x, 'e> =
    try
      f ()
    finally
      g ()

  member inline __.Combine(r, f): Result<'x, 'e> =
    Internals.combine r (f ())

  member inline __.While(guard, f): Result<unit, 'e> =
    let rec loop () =
      if guard () then
        Internals.combine (f ()) loop
      else
        Ok ()
    loop ()

  member inline __.For(xs: seq<'x>, f): Result<unit, 'e> =
    use enumerator = xs.GetEnumerator()
    let rec loop () =
      if enumerator.MoveNext() then
        Internals.combine (f enumerator.Current) loop
      else
        Ok ()
    loop ()
