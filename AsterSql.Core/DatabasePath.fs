namespace AsterSql.Core

type ColumnPath =
  {
    DatabaseName:
      string
    SchemaName:
      string
    TableName:
      string
    ColumnName:
      string
  }

type TablePath =
  {
    DatabaseName:
      string
    SchemaName:
      string
    TableName:
      string
  }
with
  static member (/) (this: TablePath, columnName): ColumnPath =
    {
      DatabaseName =
        this.DatabaseName
      SchemaName =
        this.SchemaName
      TableName =
        this.TableName
      ColumnName =
        columnName
    }

type DatabaseSchemaPath =
  {
    DatabaseName:
      string
    SchemaName:
      string
  }
with
  static member (/) (this: DatabaseSchemaPath, tableName): TablePath =
    {
      DatabaseName =
        this.DatabaseName
      SchemaName =
        this.SchemaName
      TableName =
        tableName
    }

type DatabasePath =
  {
    DatabaseName:
      string
  }
with
  static member (/) (this: DatabasePath, schemaName): DatabaseSchemaPath =
    {
      DatabaseName =
        this.DatabaseName
      SchemaName =
        schemaName
    }

type DatabasePathRoot private () =
  static member val Instance = DatabasePathRoot()

  static member (/) (this: DatabasePathRoot, databaseName) =
    {
      DatabaseName =
        databaseName
    }
