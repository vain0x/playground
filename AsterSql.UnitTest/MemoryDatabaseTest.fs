namespace AsterSql.UnitTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open AsterSql.Core

module MemoryDatabaseTest =
  type Long = int64

  let schemaName = "public"

  type PersonTable(schemaName) =
    inherit MemoryTable(schemaName, "persons")

  type TestEntity(schema: MemoryDatabaseSchema<TestEntity>) =
    inherit Entity()

    member this.Persons =
      PersonTable("persons")

    override this.Dispose() = ()

  let ``test Table.Insert`` =
    let database = new MemoryDatabase("database")
    let schema = database.GetSchema<TestEntity>("public")
    use entity = schema.Connect()
    ()
