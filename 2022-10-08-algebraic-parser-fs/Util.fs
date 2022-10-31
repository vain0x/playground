module internal Util

type Queue<'T> = System.Collections.Generic.Queue<'T>
type Stack<'T> = System.Collections.Generic.Stack<'T>
type HashSet<'T> = System.Collections.Generic.HashSet<'T>
type HashMap<'K, 'T> = System.Collections.Generic.Dictionary<'K, 'T>

let inline todo () = failwith "todo"
let inline unreachable () = failwith "unreachable"
let inline (|Unreachable|) _ = unreachable ()

let inline swap<'T> (x: 'T byref) (y: 'T byref) =
  let t = x
  x <- y
  y <- t

module HashMap =
  let inline findOr key alt (map: HashMap<_, _>) =
    match map.TryGetValue(key) with
    | true, it -> it
    | _ -> alt

  let inline tryFind key (map: HashMap<_, _>) =
    match map.TryGetValue(key) with
    | true, value -> Some value
    | _ -> None
