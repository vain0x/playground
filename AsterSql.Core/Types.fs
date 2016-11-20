namespace AsterSql.Core

type DbType =
  | TInt
  | TString

type DbValue =
  | VInt
    of int64
  | VString 
    of string

type Field =
  {
    Name
      : string
    Type
      : DbType
  }

type Record =
  array<DbValue>

type Relation =
  {
    Fields
      : array<Field>
    Records
      : array<Record>
  }

type Column<'typ>() =
  do ()

type Table<'typ>() =
  member this.Name = "name"

  member this.Add(t: 'typ) =
    ()

[<AbstractClass>]
type Database<'entity>() =
  abstract member Connect: unit -> 'entity

type MemoryDatabase<'entity>() =
  inherit Database<'entity>()

  override this.Connect(): 'entity =
    failwith ""
