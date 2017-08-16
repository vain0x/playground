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

namespace BoilerplateConstructorGenerator.CompleteConstructorGeneration
{
    public sealed class MySyntaxFactory
    {
        LanguageVersion LanguageVersion { get; }

        public MySyntaxFactory(LanguageVersion languageVersion)
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

        public sealed class AssignableVariableMember
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

        public ImmutableArray<AssignableVariableMember> Assignables(ImmutableArray<VariableMember> varMembers)
        {
            return
                varMembers
                .Where(m => !m.HasInitializer)
                .Select(m => new AssignableVariableMember(m))
                .ToImmutableArray();
        }

        public ParameterListSyntax ParameterList(ImmutableArray<AssignableVariableMember> assignables)
        {
            return
                SyntaxFactory.ParameterList(
                    SeparatedList(
                        assignables.Select(a =>
                            Parameter(a.ParameterIdentifier)
                            .WithType(TypeSyntax(a.TypeSymbol))
                        )));
        }

        public IEnumerable<StatementSyntax> ContractStatements(SemanticModel semanticModel, ImmutableArray<AssignableVariableMember> assignables)
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

        IEnumerable<ExpressionStatementSyntax> AssignmentStatements(ImmutableArray<AssignableVariableMember> assignables)
        {
            return
                assignables.Select(a =>
                    ExpressionStatement(
                        AssignmentExpression(
                            SyntaxKind.SimpleAssignmentExpression,
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                ThisExpression(),
                                IdentifierName(a.MemberName)
                            ),
                            IdentifierName(a.ParameterIdentifier)
                        )));
        }

        public ConstructorDeclarationSyntax CompleteConstructor(SemanticModel semanticModel, TypeDeclarationSyntax typeDecl, ImmutableArray<VariableMember> varMembers)
        {
            var assignables = Assignables(varMembers);
            var parameterList = ParameterList(assignables);
            var contractStatements = ContractStatements(semanticModel, assignables);
            var assignments = AssignmentStatements(assignables);
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
                .WithAdditionalAnnotations(
                    Formatter.Annotation,
                    Simplifier.Annotation
                );
        }
    }
}
