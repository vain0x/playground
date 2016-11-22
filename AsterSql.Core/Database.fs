namespace AsterSql.Core

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Extension =
  [<Extension>]
  let Insert (this: Entity, table: Table, assign: Action<IExpressionRecord>) =
    let record = DictionaryExpressionRecord()
    assign.Invoke(record)
    let statement =
      {
        TableName =
          table.TablePath.TableName
        Record =
          record
      }
