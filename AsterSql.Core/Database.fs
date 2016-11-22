namespace AsterSql.Core

open System
open System.Threading.Tasks

[<AbstractClass>]
type DatabaseSchema<'entity when 'entity :> Entity>() =
  inherit DatabaseSchema()

  abstract Connect: unit -> 'entity
  
[<AbstractClass>]
type Database() =
  abstract Path: DatabasePath

  abstract GetSchema<'entity when 'entity :> Entity> :
    string -> DatabaseSchema<'entity>

  abstract ExecuteSelect<'entity when 'entity :> Entity> :
    'entity * SelectStatement -> seq<IReadOnlyRecord>

  abstract ExecuteValueInsert<'entity when 'entity :> Entity> :
    'entity * ValueInsertStatement -> Long

  member this.Name =
    this.Path.DatabaseName
