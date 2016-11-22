namespace AsterSql.Core

open System

[<AbstractClass>]
type DatabaseSchema<'entity when 'entity :> Entity>() =
  inherit DatabaseSchema()

  abstract Connect: unit -> 'entity
  
[<AbstractClass>]
type Database() =
  abstract Path: DatabasePath

  abstract GetSchema<'entity when 'entity :> Entity> :
    string -> DatabaseSchema<'entity>

  member this.Name =
    this.Path.DatabaseName
