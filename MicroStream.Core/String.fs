namespace MicroStream

module String =
  let trySplit2 (separator: string) (str: string): option<string * string> =
    let index = str.IndexOf(separator)
    if index < 0
    then None
    else Some (str.Substring(0, index), str.Substring(index + separator.Length))
