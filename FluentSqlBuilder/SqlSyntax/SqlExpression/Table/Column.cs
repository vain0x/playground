using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class Column<TValue>
        : SqlExpression<IScalar<TValue>>
        , IColumn<TValue>
    {
        #region IColumn
        public string QualifiedName { get; }
        public string RawName { get; }

        public TValue this[DataRow row]
        {
            get
            {
                throw new NotImplementedException();
            }

            set
            {
                throw new NotImplementedException();
            }
        }

        public TValue this[IRecord record]
        {
            get
            {
                return (TValue)record.GetValueOrDefault(RawName, default(TValue));
            }
            set
            {
                record.SetValue(RawName, value);
            }
        }

        #region SqlExpression
        public override IEnumerable<string> Tokens
        {
            get
            {
                yield return QualifiedName;
            }
        }

        public override IEnumerable<DbParameter> Parameters =>
            Enumerable.Empty<DbParameter>();
        #endregion
        #endregion

        public Column(SqlBuilder sqlBuilder, ITable table, string rawName)
            : base(sqlBuilder)
        {
            RawName = rawName;
            QualifiedName = sqlBuilder.Language.BuildIdentifier(table.RawName, rawName);
        }
    }
}
