using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VainZero.EntityClassGenerator.CSharpWriters;
using VainZero.EntityClassGenerator.Domain;

namespace VainZero.EntityClassGenerator.Providers
{
    public interface IDbProvider
    {
        DbSchema GetSchema();
        ITypeNameMapper TypeNameMapper { get; }
    }
}
