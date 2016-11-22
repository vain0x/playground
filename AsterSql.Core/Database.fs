namespace AsterSql.Core

open System

[<AbstractClass>]
type DatabaseSchema<'entity when 'entity :> Entity>() =
  inherit DatabaseSchema()

  abstract Connect: unit -> 'entity
  
[<AbstractClass>]
type Database() =
  abstract Name: string

  abstract GetSchema<'entity when 'entity :> Entity> :
    string -> DatabaseSchema<'entity>
