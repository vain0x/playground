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

namespace BoilerplateConstructorGenerator.CompleteConstructors
{
    public sealed class MySyntaxFactory
    {
        const string MagicComment = "analyzer: complete-constructor";

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

        static Func<ISymbol, IdentifierNameSyntax> TypeSyntax(SemanticModel semanticModel, int position)
        {
            return symbol =>
            {
                var name = symbol.ToMinimalDisplayString(semanticModel, position);
                return IdentifierName(name);
            };
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
                ParameterIdentifier = EscapedIdentifier(member.NameAsParameter());
            }
        }

        ImmutableArray<AssignableVariableMember> AssignablesCore(ImmutableArray<VariableMember> varMembers, ImmutableHashSet<ISymbol> initializedSymbols)
        {
            return
                varMembers
                .Where(m => !m.HasInitializer && !initializedSymbols.Contains(m.SymbolBase))
                .Select(m => new AssignableVariableMember(m))
                .ToImmutableArray();
        }

        /// <summary>
        /// Gets an array of assignable variable members.
        /// A variable member is assignable if and only if
        /// it doesn't have the initializer and can't be initialized in the specified statements.
        /// </summary>
        public ImmutableArray<AssignableVariableMember> Assignables(ImmutableArray<VariableMember> varMembers, IEnumerable<StatementSyntax> statements, SemanticModel semanticModel)
        {
            var initializedSymbols = ImmutableHashSet.CreateBuilder<ISymbol>();

            foreach (var statement in statements)
            {
                if (
                    statement is ExpressionStatementSyntax expressionStatement
                    && expressionStatement.Expression is AssignmentExpressionSyntax assignment
                    && assignment.IsKind(SyntaxKind.SimpleAssignmentExpression)
                    && assignment.Left != null
                    )
                {
                    var left = semanticModel.GetSymbolInfo(assignment.Left).Symbol;
                    if (left == null) continue;
                    initializedSymbols.Add(left);
                }
            }

            return AssignablesCore(varMembers, initializedSymbols.ToImmutable());
        }

        public ImmutableArray<AssignableVariableMember> Assignables(ImmutableArray<VariableMember> varMembers)
        {
            return AssignablesCore(varMembers, ImmutableHashSet<ISymbol>.Empty);
        }

        public ParameterListSyntax ParameterList(ImmutableArray<AssignableVariableMember> assignables, Func<ISymbol, IdentifierNameSyntax> typeSyntax)
        {
            return
                SyntaxFactory.ParameterList(
                    SeparatedList(
                        assignables.Select(a =>
                            Parameter(a.ParameterIdentifier)
                            .WithType(typeSyntax(a.TypeSymbol))
                        )));
        }

        public IEnumerable<StatementSyntax> ContractStatements(SemanticModel semanticModel, ImmutableArray<AssignableVariableMember> assignables, Func<ISymbol, IdentifierNameSyntax> typeSyntax)
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
                        ObjectCreationExpression(typeSyntax(aneType))
                        .WithArgumentList(
                            ArgumentList(
                                SingletonSeparatedList(
                                    Argument(
                                        NameOfSyntax(a.ParameterIdentifier)
                                    )))));

                yield return
                    IfStatement(condition, statement)
                    .WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation);
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
                                IdentifierName(EscapedIdentifier(a.MemberName))
                            ),
                            IdentifierName(a.ParameterIdentifier)
                        ))
                    .WithAdditionalAnnotations(Formatter.Annotation, Simplifier.Annotation)
                );
        }

        public ConstructorDeclarationSyntax CompleteConstructor(SemanticModel semanticModel, TypeDeclarationSyntax typeDecl, ImmutableArray<VariableMember> varMembers)
        {
            var typeSyntax = TypeSyntax(semanticModel, typeDecl.SpanStart);
            var assignables = Assignables(varMembers);
            var parameterList = ParameterList(assignables, typeSyntax);
            var contractStatements = ContractStatements(semanticModel, assignables, typeSyntax);
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
                .WithLeadingTrivia(
                    TriviaList(
                        ElasticCarriageReturnLineFeed,
                        Comment($"// {MagicComment}"),
                        ElasticCarriageReturnLineFeed
                    ));
        }

        static bool IsArgumentNullExceptionCreationExpression(ExpressionSyntax expression, SemanticModel semanticModel)
        {
            return
                expression is ObjectCreationExpressionSyntax objectCreation
                && semanticModel.GetSymbolInfo(objectCreation.Type).Symbol is ITypeSymbol typeSymbol
                && typeSymbol.ToDisplayString() == "System.ArgumentNullException";
        }

        /// <summary>
        /// Determines if a statement matches with
        /// <c>if (... == null) throw new ArgumentNullException(...)</c>.
        /// </summary>
        static bool IsNullRejector(StatementSyntax statement, SemanticModel semanticModel)
        {
            return
                statement is IfStatementSyntax ifStatement
                && ifStatement.Condition is BinaryExpressionSyntax condition
                && condition.IsKind(SyntaxKind.EqualsExpression)
                && condition.Right.IsKind(SyntaxKind.NullLiteralExpression)
                && ifStatement.Statement is ThrowStatementSyntax body
                && IsArgumentNullExceptionCreationExpression(body.Expression, semanticModel);
        }

        static bool IsParameterSymbol(ExpressionSyntax expression, ImmutableHashSet<ISymbol> parameterSet, SemanticModel semanticModel)
        {
            var parameterSymbol = semanticModel.GetSymbolInfo(expression).Symbol;
            return parameterSymbol != null && parameterSet.Contains(parameterSymbol);
        }

        /// <summary>
        /// Determines if a statement matches with
        /// <c>M = p</c>, where <c>M</c> is a member or error and <c>p</c> is a parameter.
        /// Note that <c>p ?? ...</c> is also acceptable on the right-hand side
        /// because <c>p ?? throw new ...</c> is preferred in C# 7.
        /// </summary>
        static bool IsMemberAssignment(StatementSyntax statement, ImmutableHashSet<ISymbol> memberSet, ImmutableHashSet<ISymbol> parameterSet, SemanticModel semanticModel)
        {
            if (!(statement is ExpressionStatementSyntax expressionStatement)) return false;

            if (!(expressionStatement.Expression is AssignmentExpressionSyntax assignment
                && assignment.IsKind(SyntaxKind.SimpleAssignmentExpression)
                && assignment.Left != null
                && assignment.Right != null
                )) return false;

            var leftSymbol = semanticModel.GetSymbolInfo(assignment.Left).Symbol;
            if (leftSymbol != null && !memberSet.Contains(leftSymbol)) return false;

            if (assignment.Right.IsKind(SyntaxKind.CoalesceExpression)
                && assignment.Right is BinaryExpressionSyntax coalesceExpression
                && coalesceExpression.Left != null
                )
            {
                // Try to match with ``... = p ?? ...``.
                if (!IsParameterSymbol(coalesceExpression.Left, parameterSet, semanticModel))
                {
                    return false;
                }
            }
            else
            {
                // Try to match with ``... = p;```.
                if (!IsParameterSymbol(assignment.Right, parameterSet, semanticModel))
                {
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Tries to find complete constructor of the specified type
        /// and provide fix function.
        /// <paramref name="originalDecl"/> will be <c>null</c> if missing.
        /// <paramref name="fix"/> will be <c>null</c> if missing or no change.
        /// </summary>
        public void
            FixCompleteConstructor(
                SemanticModel semanticModel,
                TypeDeclarationSyntax typeDecl,
                ImmutableArray<VariableMember> varMembers,
                out ConstructorDeclarationSyntax originalDecl,
                out Func<ConstructorDeclarationSyntax> fix
            )
        {
            var typeSyntax = TypeSyntax(semanticModel, typeDecl.SpanStart);

            var constructorDecls =
                typeDecl.Members
                .OfType<ConstructorDeclarationSyntax>()
                .Where(c =>
                    c.GetLeadingTrivia().Any(t =>
                        t.IsComment() && t.ToString().Contains(MagicComment)
                    )
                    && c.Initializer == null
                    && c.ParameterList != null
                    && c.ParameterList.Parameters.Count > 0
                    && c.Body != null
                    && c.Body.Statements.Count > 0
                )
                .ToImmutableArray();

            if (constructorDecls.Length > 0)
            {
                var memberSet =
                    varMembers
                    .Select(vm => vm.SymbolBase)
                    .ToImmutableHashSet();

                foreach (var constructorDecl in constructorDecls)
                {
                    var body = constructorDecl.Body;

                    var parameters =
                        constructorDecl.ParameterList.Parameters
                        .Select(p =>
                            new
                            {
                                parameter = p,
                                symbol = semanticModel.GetDeclaredSymbol(p),
                                typeSymbol =
                                    semanticModel.GetSymbolInfo(p.Type).Symbol as ITypeSymbol,
                            })
                        .ToImmutableArray();

                    if (parameters.Any(p => p.symbol == null || p.typeSymbol == null)) continue;

                    var parameterSet =
                        parameters.Select(a => a.symbol).ToImmutableHashSet<ISymbol>();

                    var generatedStatementCount =
                        body.Statements
                        .TakeWhile(s =>
                            IsNullRejector(s, semanticModel)
                            || IsMemberAssignment(s, memberSet, parameterSet, semanticModel)
                            )
                        .Count();
                    if (generatedStatementCount == 0) continue;

                    var assignables =
                        Assignables(
                            varMembers,
                            body.Statements.Skip(generatedStatementCount),
                            semanticModel
                        );

                    if (
                        parameters.Select(a => new { name = a.symbol.Name, a.typeSymbol })
                        .SequenceEqual(assignables.Select(a => new { name = a.ParameterIdentifier.Text, typeSymbol = a.TypeSymbol }))
                        )
                    {
                        originalDecl = constructorDecl;
                        fix = null;
                        return;
                    }

                    var assignments =
                        AssignmentStatements(assignables).Select((statement, index) =>
                        {
                            // Insert additional linebreaks. NOTE: Not affected?
                            if (index == 0 && generatedStatementCount < body.Statements.Count)
                            {
                                return
                                    statement.WithTrailingTrivia(
                                        statement.GetTrailingTrivia()
                                        .Add(CarriageReturnLineFeed)
                                        .Insert(0, CarriageReturnLineFeed)
                                    );
                            }
                            return statement;
                        });

                    fix = () =>
                        constructorDecl
                        .WithParameterList(ParameterList(assignables, typeSyntax))
                        .WithBody(
                            body.WithStatements(
                                List(
                                    ContractStatements(semanticModel, assignables, typeSyntax)
                                    .Concat(assignments)
                                    .Concat(body.Statements.Skip(generatedStatementCount))
                                )));

                    originalDecl = constructorDecl;
                    return;
                }
            }

            originalDecl = default(ConstructorDeclarationSyntax);
            fix = default(Func<ConstructorDeclarationSyntax>);
            return;
        }

        public bool
            HasCompleteConstructor(
                SemanticModel semanticModel,
                TypeDeclarationSyntax typeDecl,
                ImmutableArray<VariableMember> varMembers
            )
        {
            FixCompleteConstructor(semanticModel, typeDecl, varMembers, out var constructorDecl, out var _);
            return constructorDecl != null;
        }
    }
}
