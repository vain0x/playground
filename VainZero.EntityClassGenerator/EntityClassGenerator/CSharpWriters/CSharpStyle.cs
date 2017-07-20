using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.EntityClassGenerator.CSharpWriters
{
    public sealed class CSharpStyle
    {
        public string Namespace { get; }

        public CSharpStyle(string @namespace)
        {
            Namespace = @namespace;
        }
    }
}
