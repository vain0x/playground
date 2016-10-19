using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;

namespace AsterSql.Data.Fake
{
    [DebuggerDisplay("{\"{ParameterName}\" = {Value}: {DbType}")]
    public class FakeDbParameter
        : DbParameter
    {
        public override DbType DbType { get; set; }
        public override ParameterDirection Direction { get; set; }
        public override bool IsNullable { get; set; }
        public override string ParameterName { get; set; }
        public override int Size { get; set; }
        public override string SourceColumn { get; set; }
        public override bool SourceColumnNullMapping { get; set; }
        public override object Value { get; set; }

        public override void ResetDbType()
        {
            DbType = DbType.String;
        }
    }

    public class FakeDbParameterCollection
        : DbParameterCollection
    {
        List<DbParameter> Parameters { get; } =
            new List<DbParameter>();

        public override object SyncRoot { get; } = new object();

        public override int Count => Parameters.Count;

        public override int Add(object value)
        {
            Parameters.Add((DbParameter)value);
            return Count - 1;
        }

        public override void AddRange(Array values)
        {
            for (var i = 0; i < values.Length; i ++)
            {
                Add(values.GetValue(i));
            }
        }

        public override void Clear()
        {
            throw new NotImplementedException();
        }

        public override bool Contains(string value)
        {
            throw new NotImplementedException();
        }

        public override bool Contains(object value)
        {
            throw new NotImplementedException();
        }

        public override void CopyTo(Array array, int index)
        {
            throw new NotImplementedException();
        }

        public override IEnumerator GetEnumerator()
        {
            return Parameters.GetEnumerator();
        }

        public override int IndexOf(string parameterName)
        {
            throw new NotImplementedException();
        }

        public override int IndexOf(object value)
        {
            throw new NotImplementedException();
        }

        public override void Insert(int index, object value)
        {
            throw new NotImplementedException();
        }

        public override void Remove(object value)
        {
            throw new NotImplementedException();
        }

        public override void RemoveAt(string parameterName)
        {
            throw new NotImplementedException();
        }

        public override void RemoveAt(int index)
        {
            throw new NotImplementedException();
        }

        protected override DbParameter GetParameter(string parameterName)
        {
            throw new NotImplementedException();
        }

        protected override DbParameter GetParameter(int index)
        {
            return Parameters[index];
        }

        protected override void SetParameter(string parameterName, DbParameter value)
        {
            throw new NotImplementedException();
        }

        protected override void SetParameter(int index, DbParameter value)
        {
            throw new NotImplementedException();
        }
    }

    public class FakeDbCommand
        : DbCommand
    {
        public override string CommandText { get; set; }
        
        public override void Cancel()
        {
            throw new NotImplementedException();
        }

        public override int CommandTimeout { get; set; }

        public override CommandType CommandType { get; set; }

        protected override DbParameter CreateDbParameter()
        {
            return new FakeDbParameter();
        }

        protected override DbConnection DbConnection { get; set; }

        protected override DbParameterCollection DbParameterCollection { get; } =
            new FakeDbParameterCollection();

        protected override DbTransaction DbTransaction { get; set; }

        public override bool DesignTimeVisible { get; set; }

        protected override DbDataReader ExecuteDbDataReader(CommandBehavior behavior)
        {
            throw new NotImplementedException();
        }

        public override Task<int> ExecuteNonQueryAsync(CancellationToken cancellationToken)
        {
            throw new NotImplementedException();
        }

        public override int ExecuteNonQuery()
        {
            throw new NotImplementedException();
        }

        public override object ExecuteScalar()
        {
            throw new NotImplementedException();
        }

        public override void Prepare()
        {
            throw new NotImplementedException();
        }

        public override UpdateRowSource UpdatedRowSource { get; set; }
    }

    public class FakeDbProviderFactory
        : DbProviderFactory
    {
        public override bool CanCreateDataSourceEnumerator => false;

        public override DbCommand CreateCommand()
        {
            return new FakeDbCommand();
        }

        public override DbParameter CreateParameter()
        {
            return new FakeDbParameter();
        }
    }
}
