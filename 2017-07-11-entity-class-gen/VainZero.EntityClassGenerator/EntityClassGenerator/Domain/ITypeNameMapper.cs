using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.EntityClassGenerator.Domain
{
    public interface ITypeNameMapper
    {
        string CSharpTypeName(DbColumn column);
    }
}
