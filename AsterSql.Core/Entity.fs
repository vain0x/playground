namespace AsterSql.Core

open System
open System.Reflection

[<AbstractClass>]
type Entity() as this =
  let tables =
    lazy (
      this.GetType().GetProperties()
      |> Array.choose
        (fun pi ->
          if typeof<Table>.IsAssignableFrom(pi.PropertyType)
            && pi.GetMethod |> isNull |> not
          then
            let table = pi.GetValue(this) :?> Table
            (table.TablePath.TableName, table) |> Some
          else
            None
        )
      |> Map.ofSeq
    )

  member this.TryGetTable(tableName) =
    tables.Value |> Map.tryFind tableName

  abstract Dispose: unit -> unit

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Entity =
  ()
