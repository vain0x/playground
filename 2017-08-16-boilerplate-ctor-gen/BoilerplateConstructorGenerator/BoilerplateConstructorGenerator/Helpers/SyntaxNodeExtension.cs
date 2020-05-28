using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Simplification;

namespace BoilerplateConstructorGenerator
{
    public static class SyntaxNodeExtension
    {
        public static TypeDeclarationSyntax WithMembers(this TypeDeclarationSyntax typeDecl, SyntaxList<MemberDeclarationSyntax> members)
        {
            switch (typeDecl)
            {
                case ClassDeclarationSyntax classDecl:
                    return classDecl.WithMembers(members);
                case StructDeclarationSyntax structDecl:
                    return structDecl.WithMembers(members);
                case InterfaceDeclarationSyntax interfaceDecl:
                    return interfaceDecl.WithMembers(members);
                default:
                    throw new InvalidOperationException();
            }
        }

        public static TypeDeclarationSyntax AddMembers(this TypeDeclarationSyntax typeDecl, params MemberDeclarationSyntax[] members)
        {
            switch (typeDecl)
            {
                case ClassDeclarationSyntax classDecl:
                    return classDecl.AddMembers(members);
                case StructDeclarationSyntax structDecl:
                    return structDecl.AddMembers(members);
                case InterfaceDeclarationSyntax interfaceDecl:
                    return interfaceDecl.AddMembers(members);
                default:
                    throw new InvalidOperationException();
            }
        }
    }
}
