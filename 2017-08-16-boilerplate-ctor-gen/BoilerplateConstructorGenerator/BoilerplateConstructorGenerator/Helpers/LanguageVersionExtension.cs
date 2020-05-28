using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp;

namespace BoilerplateConstructorGenerator
{
    public static class LanguageVersionExtension
    {
        public static bool SupportsNameOf(this LanguageVersion @this)
        {
            switch (@this)
            {
                case LanguageVersion.CSharp1:
                case LanguageVersion.CSharp2:
                case LanguageVersion.CSharp3:
                case LanguageVersion.CSharp4:
                case LanguageVersion.CSharp5:
                    return false;
                default:
                    return true;
            }
        }
    }
}
