using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;

namespace BoilerplateConstructorGenerator
{
    public static class SyntaxTriviaExtension
    {
        public static bool IsComment(this SyntaxTrivia @this)
        {
            return
                @this.IsKind(SyntaxKind.SingleLineCommentTrivia)
                || @this.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia)
                || @this.IsKind(SyntaxKind.MultiLineCommentTrivia)
                || @this.IsKind(SyntaxKind.MultiLineDocumentationCommentTrivia);
        }
    }
}
