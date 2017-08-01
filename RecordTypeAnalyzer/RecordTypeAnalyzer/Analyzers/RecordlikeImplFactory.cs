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
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace RecordTypeAnalyzer.Analyzers
{
    public sealed class RecordlikeImplFactory
    {
        const string RegionHeader = RegionHeaderKeyword + " v" + Version;
        const string RegionHeaderKeyword = "Record-like Implementation";
        const string Version = "1.0.0";

        LanguageVersion LanguageVersion { get; }

        public RecordlikeImplFactory(LanguageVersion languageVersion)
        {
            LanguageVersion = languageVersion;
        }

        ExpressionSyntax NameOfSyntax(SyntaxToken identifier)
        {
            if (LanguageVersion.SupportsNameOf())
            {
                return
                    InvocationExpression(
                        IdentifierName("nameof"),
                        ArgumentList(
                            SingletonSeparatedList(
                                Argument(
                                    IdentifierName(identifier)
                                ))));
            }

            return
                LiteralExpression(
                    SyntaxKind.StringLiteralExpression,
                    Literal(identifier.Text)
                );
        }

        #region Escape
        static ImmutableHashSet<string> KeywordStrings { get; } =
            SyntaxFacts.GetContextualKeywordKinds()
            .Concat(SyntaxFacts.GetKeywordKinds())
            .Select(kind => Token(kind).Text)
            .ToImmutableHashSet();

        static SyntaxToken EscapedIdentifier(string name)
        {
            if (KeywordStrings.Contains(name))
            {
                return VerbatimIdentifier(SyntaxTriviaList.Empty, name, name, SyntaxTriviaList.Empty);
            }
            return Identifier(name);
        }
        #endregion

        static TypeSyntax TypeSyntax(ISymbol symbol)
        {
            var name = symbol.ToDisplayString();
            return IdentifierName(name);
        }

        sealed class AssignableVariableMember
        {
            VariableMember Member { get; }

            public string MemberName => Member.SymbolBase.Name;

            public SyntaxToken ParameterIdentifier { get; }

            public ITypeSymbol TypeSymbol => Member.TypeSymbol;

            public AssignableVariableMember(VariableMember member)
            {
                Member = member;
                ParameterIdentifier = EscapedIdentifier(member.NameAsCamelCase());
            }
        }

        ConstructorDeclarationSyntax Constructor(SemanticModel semanticModel, TypeDeclarationSyntax typeDecl, ImmutableArray<VariableMember> varMembers)
        {
            var assignables =
                varMembers
                .Where(m => !m.HasInitializer)
                .Select(m => new AssignableVariableMember(m))
                .ToImmutableArray();

            IEnumerable<StatementSyntax> ContractStatements()
            {
                var aneType = semanticModel.Compilation.GetTypeByMetadataName(typeof(ArgumentNullException).FullName);
                if (aneType == null) yield break;

                foreach (var a in assignables)
                {
                    if (!a.TypeSymbol.IsReferenceType) continue;

                    var condition =
                        BinaryExpression(
                            SyntaxKind.EqualsExpression,
                            IdentifierName(a.ParameterIdentifier),
                            LiteralExpression(SyntaxKind.NullLiteralExpression)
                        );
                    var statement =
                        ThrowStatement(
                            ObjectCreationExpression(TypeSyntax(aneType))
                            .WithArgumentList(
                                ArgumentList(
                                    SingletonSeparatedList(
                                        Argument(
                                            NameOfSyntax(a.ParameterIdentifier)
                                        )))));

                    yield return IfStatement(condition, statement);
                }
            }

            var parameterList =
                ParameterList(
                    SeparatedList(
                        assignables.Select(a =>
                            Parameter(a.ParameterIdentifier)
                            .WithType(TypeSyntax(a.TypeSymbol))
                        )));

            var contractStatements = ContractStatements();

            var assignments =
                assignables.Select(a =>
                    ExpressionStatement(
                        AssignmentExpression(
                            SyntaxKind.SimpleAssignmentExpression,
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                ThisExpression(),
                                IdentifierName(a.MemberName)
                            ).WithAdditionalAnnotations(Simplifier.Annotation),
                            IdentifierName(a.ParameterIdentifier)
                        )));

            var body = Block(contractStatements.Concat(assignments));

            var typeName = typeDecl.Identifier.Text;
            return
                ConstructorDeclaration(typeName)
                .WithModifiers(
                    SyntaxTokenList.Create(
                        Token(SyntaxKind.PublicKeyword)
                    ))
                .WithParameterList(parameterList)
                .WithBody(body)
                .WithAdditionalAnnotations(Formatter.Annotation);
        }

        RegionDirectiveTriviaSyntax RegionDirective()
        {
            return
                RegionDirectiveTrivia(isActive: true)
                .WithEndOfDirectiveToken(
                    Token(
                        TriviaList(
                            Whitespace(" "),
                            PreprocessingMessage(RegionHeader)
                        ),
                        SyntaxKind.EndOfDirectiveToken,
                        TriviaList()
                    ));
        }

        public MemberDeclarationSyntax[] Members(SemanticModel semanticModel, TypeDeclarationSyntax typeDecl, ImmutableArray<VariableMember> varMembers)
        {
            var members = new List<MemberDeclarationSyntax>
            {
                Constructor(semanticModel, typeDecl, varMembers),
            };

            // Enclose with a region.
            if (members.Count > 0)
            {
                var first = members[0];
                members[0] =
                    first.WithLeadingTrivia(
                        first.GetLeadingTrivia().InsertRange(
                            0,
                            new[]
                            {
                                EndOfLine(Environment.NewLine),
                                Trivia(RegionDirective()),
                            }));

                var last = members[members.Count - 1];
                members[members.Count - 1] =
                    last.WithTrailingTrivia(
                        last.GetTrailingTrivia().AddRange(
                            new[]
                            {
                                Trivia(EndRegionDirectiveTrivia(isActive: true)),
                                EndOfLine(Environment.NewLine),
                            }));
            }

            return members.ToArray();
        }
    }
}
