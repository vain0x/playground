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
        public string ContextName { get; }

        public CSharpStyle(string @namespace, string contextName)
        {
            Namespace = @namespace;
            ContextName = contextName;
        }
    }
}
