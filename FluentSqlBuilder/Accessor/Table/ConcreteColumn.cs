using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public sealed class ConcreteColumn<TValue>
        : Column<TValue>
    {
        #region IColumn
        public override string QualifiedName { get; }
        public override string UniqueName { get; }
        public override string RawName { get; }
        
        public override DbType DbType
        {
            get
            {
                return SqlBuilder.Provider.DbTypeFromType(typeof(TValue));
            }
        }

        public override TValue this[DataRow row]
        {
            get
            {
                return row.Field<TValue>(UniqueName);
            }

            set
            {
                row.SetField(UniqueName, value);
            }
        }

        public override TValue this[IValueRecord record]
        {
            get
            {
                return (TValue)record[UniqueName];
            }
            set
            {
                record[UniqueName] = value;
            }
        }

        public override SqlExpression<IScalar<TValue>> this[IExpressionRecord record]
        {
            get
            {
                return record[UniqueName].Unbox<TValue>();
            }
            set
            {
                record[UniqueName] = value.Box();
            }
        }

        #region SqlPart
        internal override IEnumerable<string> Tokens
        {
            get
            {
                yield return QualifiedName;
            }
        }

        internal override IEnumerable<DbParameter> Parameters =>
            Enumerable.Empty<DbParameter>();
        #endregion
        #endregion

        public ConcreteColumn(SqlBuilder sqlBuilder, Table table, string rawName)
            : base(sqlBuilder)
        {
            RawName = rawName;
            UniqueName = SqlBuilder.Language.ConcatIdentifiers(table.Alias, RawName);
            QualifiedName = sqlBuilder.Language.BuildColumnName(table.Alias, rawName);
        }
    }
}
