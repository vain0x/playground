namespace AsterSql.Core

open System
open System.Reflection

[<AbstractClass>]
type Entity() =
  abstract Dispose: unit -> unit

  interface IDisposable with
    override this.Dispose() =
      this.Dispose()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Entity =
  ()
