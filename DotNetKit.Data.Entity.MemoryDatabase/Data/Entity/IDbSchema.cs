using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Data.Entity
{
    /// <summary>
    /// Represents a database schema.
    /// </summary>
    public interface IDbSchema
    {
        /// <summary>
        /// Connects the schema.
        /// </summary>
        /// <returns></returns>
        DbContext Connect();
    }
}
