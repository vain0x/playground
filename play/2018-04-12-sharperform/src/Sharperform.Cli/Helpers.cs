using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using IO = System.IO;

namespace Sharperform
{
    internal static class Helper
    {
        public static T Tap<T>(this T self, Action<T> action)
        {
            action(self);
            return self;
        }

        public static IO.TextWriter TapToWriteLine(this IO.TextWriter self, string message)
        {
            self.WriteLine(message);
            return self;
        }

        public static string JoinMap<T>(this IEnumerable<T> self, string separator, Func<T, string> selector)
        {
            return string.Join(separator, self.Select(selector));
        }

        public static IEnumerable<U> SelectMany<T, U>(
            this IEnumerable<T> self,
            Func<T, Optional<U>> selector
        )
        {
            foreach (var x in self)
            {
                var yo = selector(x);
                if (yo.HasValue)
                {
                    yield return yo.Value;
                }
            }
        }
    }

    internal static class LanguageVersionExtension
    {
        public static bool SupportsNameOf(this LanguageVersion self)
        {
            switch (self)
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

    internal static class SyntaxTriviaExtension
    {
        public static bool IsComment(this SyntaxTrivia self)
        {
            return
                self.IsKind(SyntaxKind.SingleLineCommentTrivia)
                || self.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia)
                || self.IsKind(SyntaxKind.MultiLineCommentTrivia)
                || self.IsKind(SyntaxKind.MultiLineDocumentationCommentTrivia);
        }
    }

    internal static class SyntaxNodeExtension
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
