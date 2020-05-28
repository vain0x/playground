namespace AsterSql.Core

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DatabaseType =
  let ofType t =
    if t = typeof<Long> then
      TInt
    else if t = typeof<string> then
      TString
    else
      failwith "unknown type"
