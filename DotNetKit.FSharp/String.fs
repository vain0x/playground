namespace DotNetKit.FSharp

open System

[<RequireQualifiedAccess>]
module String =
  let private indexToOption (index: int) =
    if index < 0
    then Some index
    else None

  let toSeq (this: string) =
    seq {
      for i in 0..(this.Length - 1) do
        yield this.Chars(i)
    }

  let toCharArray (this: string) =
    this.ToCharArray()

  let isNullOrEmpty (this: string) =
    String.IsNullOrEmpty(this)

  let isNullOrWhitespace (this: string) =
    String.IsNullOrWhiteSpace(this)

  let startsWith value (this: string) =
    this.StartsWith(value)

  let endsWith value (this: string) =
    this.EndsWith(value)

  let tryIndexOf (value: string) (this: string) =
    this.IndexOf(value) |> indexToOption

  let tryIndexOfFrom (startIndex: int) (value: string) (this: string) =
    this.IndexOf(value, startIndex) |> indexToOption

  let tryLastIndexOf (value: string) (this: string) =
    this.LastIndexOf(value) |> indexToOption

  let tryLastIndexOfFrom (startIndex: int) (value: string) (this: string) =
    this.LastIndexOf(value, startIndex) |> indexToOption

  let tryFindIndex (predicate: char -> bool) (this: string) =
    this |> toSeq |> Seq.tryFindIndex predicate

  let substring (index: int) (count: int) (this: string) =
    this.Substring(index, count)

  let take (count: int) (this: string) =
    this.Substring(0, count)

  let takeWhile (predicate: char -> bool) (this: string) =
    match this |> tryFindIndex (predicate >> not) with
    | Some index ->
      this.Substring(0, index)
    | None ->
      this

  let truncate (maxCount: int) (this: string) =
    this.Substring(0, maxCount |> min this.Length)

  let skip (index: int) (this: string) =
    this.Remove(index)

  let skipWhile (predicate: char -> bool) (this: string) =
    match this |> tryFindIndex (predicate >> not) with
    | Some index ->
      this.Remove(index)
    | None ->
      ""

  let insertRange (index: int) (value: string) (this: string) =
    this.Insert(index, value)

  let removeRange (startIndex: int) (count: int) (this: string) =
    this.Remove(startIndex, count)

  let replaceChar (source: char) (target: char) (this: string) =
    this.Replace(source, target)

  let replace (source: string) (target: string) (this: string) =
    this.Replace(source, target)

  let toLower (this: string) =
    this.ToLower()

  let toUpper (this: string) =
    this.ToUpper()

  let trim (this: string) =
    this.Trim()

  let trimChars (chars: array<char>) (this: string) =
    this.Trim(chars)

  let trimStartChars (chars: array<char>) (this: string) =
    this.TrimStart(chars)

  let trimEndChars (chars: array<char>) (this: string) =
    this.TrimEnd(chars)

  let splitBy (separator: string) (this: string) =
    this.Split([| separator |], StringSplitOptions.None)

  let splitAt (index: int) (this: string) =
    if 0 <= index && index <= this.Length then
      (this.Substring(0, index), this.Substring(index))
    else
      ArgumentOutOfRangeException("index") |> raise
