namespace VainZero.Utf8JsonExtensions

open System
open System.Reflection
open Utf8Json
open Microsoft.FSharp.Core
open Microsoft.FSharp.Collections

type FSharpOptionFormatter<'T>() =
  interface IJsonFormatter<option<'T>> with
    override __.Serialize(writer: byref<JsonWriter>, value: option<'T>, resolver: IJsonFormatterResolver) =
      match value with
      | None ->
        writer.WriteNull()
      | Some value ->
        let formatter = resolver.GetFormatterWithVerify<'T>()
        formatter.Serialize(&writer, value, resolver)

    override __.Deserialize(reader: byref<JsonReader>, resolver: IJsonFormatterResolver) =
      if reader.ReadIsNull() then
        None
      else
        let formatter = resolver.GetFormatterWithVerify<'T>()
        let value = formatter.Deserialize(&reader, resolver)
        Some value

module FSharpFormatterCacheHelper =
  let isFSharpOptionType (t: Type) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

  let createFormatterNongeneric (t: Type) =
    let ti = t.GetTypeInfo()
    if ti.IsGenericType |> not then
      null
    else
      let genericType = ti.GetGenericTypeDefinition()
      if genericType |> isFSharpOptionType then
        Activator.CreateInstance
          ( typedefof<FSharpOptionFormatter<_>>.MakeGenericType(ti.GetGenericArguments())
          , Array.empty
          )
      else
        null

  let createFormatter<'T> () =
    createFormatterNongeneric typeof<'T> :?> IJsonFormatter<'T>

[<AbstractClass; Sealed>]
type FSharpFormatterCache<'T> private () =
  static member val Formatter = FSharpFormatterCacheHelper.createFormatter<'T>()

type FSharpFormatterResolver(parent: IJsonFormatterResolver) =
  interface IJsonFormatterResolver with
    override __.GetFormatter<'T>() =
      match FSharpFormatterCache<'T>.Formatter with
      | null ->
        parent.GetFormatter<'T>()
      | formatter ->
        formatter
