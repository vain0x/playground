using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CSharpDowngradeAnalyzer
{
    public static class SyntaxNodeExtension
    {
        public static TypeDeclarationSyntax WithMembers(this TypeDeclarationSyntax @this, SyntaxList<MemberDeclarationSyntax> members)
        {
            switch (@this)
            {
                case ClassDeclarationSyntax classDecl:
                    return classDecl.WithMembers(members);
                case StructDeclarationSyntax structDecl:
                    return structDecl.WithMembers(members);
                case InterfaceDeclarationSyntax interfaceDecl:
                    return interfaceDecl.WithMembers(members);
                default:
                    throw new ArgumentException(nameof(@this));
            }
        }
    }
}
