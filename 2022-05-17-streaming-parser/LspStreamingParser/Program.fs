// non-meta = [^:\r\n]
// non-newline = [^\r\n]
//
// field = non-meta+ ':' non-newline* '\r'? '\n'
// root = field* '\r'? '\n' body

type State =
  | HeadEmpty
  | HeadInKey
  | HeadInValue
  | HeadAfterEmptyCr
  | HeadAfterFieldCr
  | Body

let rec folder state (c: char) =
  printf "c=%A    " c

  match state, c with
  | HeadEmpty, ':' -> failwith "Expected key before ':'"
  | HeadEmpty, '\r' -> HeadAfterEmptyCr
  | (HeadEmpty
    | HeadAfterEmptyCr),
    '\n' -> Body
  | HeadEmpty, _ -> HeadInKey

  | HeadInKey, ':' -> HeadInValue
  | HeadInKey,
    ('\r'
    | '\n') -> failwith "Expected ':'"
  | HeadInKey, _ -> HeadInKey // consume key

  | HeadInValue, '\r' -> HeadAfterFieldCr // value ended
  | HeadInValue, '\n' -> HeadEmpty
  | HeadInValue, _ -> state // consume value

  | HeadAfterFieldCr, '\n' -> HeadEmpty

  | HeadAfterEmptyCr, _
  | HeadAfterFieldCr, _ -> failwith "Expected LF"

  | Body, _ -> Body

for state in
  (stdin.ReadToEnd().ToCharArray()
   |> Seq.ofArray
   |> Seq.scan folder HeadEmpty) do
  printfn "%A" state
