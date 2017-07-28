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

namespace RecordTypeAnalyzer.Analyzers
{
    public abstract class VariableMember
    {
        VariableMember()
        {
        }

        public abstract ISymbol SymbolBase { get; }

        public abstract SyntaxNode DeclBase { get; }

        public abstract ITypeSymbol TypeSymbol { get; }

        public abstract bool IsReadOnly { get; }

        public abstract bool HasInitializer { get; }

        public string NameAsCamelCase()
        {
            var name = SymbolBase.Name;
            if (name.Length > 0 && char.IsUpper(name[0]))
            {
                return char.ToLowerInvariant(name[0]) + name.Substring(1);
            }
            return name;
        }

        public sealed class Field
            : VariableMember
        {
            public IFieldSymbol Symbol { get; }

            public FieldDeclarationSyntax FieldDecl { get; }

            public VariableDeclaratorSyntax VarDecl { get; }

            public override ISymbol SymbolBase => Symbol;

            public override SyntaxNode DeclBase => VarDecl;

            public Field(IFieldSymbol symbol, FieldDeclarationSyntax fieldDecl, VariableDeclaratorSyntax varDecl)
            {
                Symbol = symbol;
                FieldDecl = fieldDecl;
                VarDecl = varDecl;
            }

            public override ITypeSymbol TypeSymbol =>
                Symbol.Type;

            public override bool IsReadOnly =>
                Symbol.IsReadOnly;

            public override bool HasInitializer =>
                VarDecl.Initializer != null;
        }

        public sealed class Property
            : VariableMember
        {
            public IPropertySymbol Symbol { get; }

            public PropertyDeclarationSyntax Decl { get; }

            public override ISymbol SymbolBase => Symbol;

            public override SyntaxNode DeclBase => Decl;

            public Property(IPropertySymbol symbol, PropertyDeclarationSyntax decl)
            {
                Symbol = symbol;
                Decl = decl;
            }

            public override ITypeSymbol TypeSymbol =>
                Symbol.Type;

            public override bool IsReadOnly =>
                Symbol.SetMethod == null
                || (Symbol.DeclaredAccessibility != Accessibility.Private
                    && Symbol.SetMethod.DeclaredAccessibility == Accessibility.Private);

            public override bool HasInitializer =>
                Decl.Initializer != null;
        }
    }

    public sealed class VariableMemberCollector
    {
        SemanticModel SemanticModel { get; }

        public VariableMemberCollector(SemanticModel semanticModel)
        {
            SemanticModel = semanticModel;
        }

        public ImmutableArray<VariableMember> Collect(TypeDeclarationSyntax typeDecl)
        {
            var varMembers = ImmutableArray.CreateBuilder<VariableMember>();

            foreach (var member in typeDecl.Members)
            {
                switch (member)
                {
                    case FieldDeclarationSyntax fieldDecl:
                        {
                            foreach (var varDecl in fieldDecl.Declaration.Variables)
                            {
                                var symbol = SemanticModel.GetDeclaredSymbol(varDecl) as IFieldSymbol;
                                if (symbol == null) continue;
                                if (symbol.IsStatic) continue;

                                varMembers.Add(new VariableMember.Field(symbol, fieldDecl, varDecl));
                            }
                            break;
                        }
                    case PropertyDeclarationSyntax propertyDecl:
                        {
                            if (propertyDecl.AccessorList == null) continue;
                            if (propertyDecl.AccessorList.Accessors.Any(a => a.Body != null)) continue;

                            var symbol = SemanticModel.GetDeclaredSymbol(propertyDecl) as IPropertySymbol;
                            if (symbol == null || symbol.IsStatic || symbol.GetMethod == null) continue;

                            varMembers.Add(new VariableMember.Property(symbol, propertyDecl));
                            break;
                        }
                }
            }

            return varMembers.ToImmutable();
        }
    }

    public sealed class RecordTypeGeneratorAnalyzer
    {
        SyntaxNodeAnalysisContext AnalysisContext { get; }

        SemanticModel SemanticModel => AnalysisContext.SemanticModel;

        CancellationToken CancellationToken => AnalysisContext.CancellationToken;

        TypeDeclarationSyntax TypeDecl { get; }

        public RecordTypeGeneratorAnalyzer(SyntaxNodeAnalysisContext analysisContext, TypeDeclarationSyntax typeDecl)
        {
            AnalysisContext = analysisContext;
            TypeDecl = typeDecl;
        }

        bool InheritsFromDefault(ITypeSymbol symbol)
        {
            if (symbol.IsValueType) return true;
            return
                symbol.BaseType == null
                || symbol.BaseType == SemanticModel.Compilation.ObjectType;
        }

        public void Analyze()
        {
            var typeSymbol = SemanticModel.GetDeclaredSymbol(TypeDecl);
            if (typeSymbol == null) return;

            if (typeSymbol.IsAbstract
                || typeSymbol.IsStatic
                || (typeSymbol.IsReferenceType && !typeSymbol.IsSealed)
                || !InheritsFromDefault(typeSymbol))
            {
                return;
            }

            var varMembers = new VariableMemberCollector(SemanticModel).Collect(TypeDecl);

            // Don't generate record-like implementation for empty types.
            if (!varMembers.Any()) return;

            // Mutable type can't be record-like.
            var isImmutable = varMembers.All(m => m.IsReadOnly);
            if (!isImmutable) return;

            AnalysisContext.ReportDiagnostic(Diagnostic.Create(DiagnosticProvider.ImplGeneration, TypeDecl.GetLocation(), TypeDecl.Identifier.Text));
        }
    }

    public sealed class RecordTypeGenerator
    {
        sealed class AssignableVariableMember
        {
            #region Escape
            static ImmutableHashSet<string> KeywordStrings { get; } =
                SyntaxFacts.GetContextualKeywordKinds()
                .Concat(SyntaxFacts.GetKeywordKinds())
                .Select(kind => SyntaxFactory.Token(kind).Text)
                .ToImmutableHashSet();

            static SyntaxToken EscapedIdentifier(string name)
            {
                if (KeywordStrings.Contains(name))
                {
                    return SyntaxFactory.VerbatimIdentifier(SyntaxTriviaList.Empty, name, name, SyntaxTriviaList.Empty);
                }
                return SyntaxFactory.Identifier(name);
            }
            #endregion

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

        sealed class ConstructorGenerator
        {
            RecordTypeGenerator Parent { get; }

            ImmutableArray<AssignableVariableMember> Assignables { get; }

            TypeSyntax TypeSyntax(ISymbol symbol)
            {
                var name = symbol.ToDisplayString();
                return SyntaxFactory.IdentifierName(name);
            }

            IEnumerable<StatementSyntax> ConstractStatements()
            {
                var aneType = Parent.SemanticModel.Compilation.GetTypeByMetadataName(typeof(ArgumentNullException).FullName);
                if (aneType == null) yield break;

                foreach (var a in Assignables)
                {
                    if (!a.TypeSymbol.IsReferenceType) continue;

                    var condition =
                        SyntaxFactory.BinaryExpression(
                            SyntaxKind.EqualsExpression,
                            SyntaxFactory.IdentifierName(a.ParameterIdentifier),
                            SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression)
                        );
                    var statement =
                        SyntaxFactory.ThrowStatement(
                            SyntaxFactory.ObjectCreationExpression(
                                TypeSyntax(aneType)
                                .WithAdditionalAnnotations(Simplifier.Annotation)
                            )
                            .WithArgumentList(
                                SyntaxFactory.ArgumentList()
                                .AddArguments(SyntaxFactory.Argument(Parent.NameOfSyntax(a.ParameterIdentifier)))
                            ));

                    yield return SyntaxFactory.IfStatement(condition, statement);
                }
            }

            public ConstructorDeclarationSyntax Generate()
            {
                var parameterList =
                    SyntaxFactory.ParameterList(
                        SyntaxFactory.SeparatedList(
                            Assignables.Select(a =>
                                SyntaxFactory.Parameter(a.ParameterIdentifier)
                                .WithType(TypeSyntax(a.TypeSymbol))
                            )));

                var constracts = ConstractStatements();

                var assignments =
                    Assignables.Select(a =>
                        SyntaxFactory.ExpressionStatement(
                            SyntaxFactory.AssignmentExpression(
                                SyntaxKind.SimpleAssignmentExpression,
                                SyntaxFactory.MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    SyntaxFactory.ThisExpression(),
                                    SyntaxFactory.IdentifierName(a.MemberName)
                                ).WithAdditionalAnnotations(Simplifier.Annotation),
                                SyntaxFactory.IdentifierName(a.ParameterIdentifier)
                            )));

                var body = SyntaxFactory.Block(constracts.Concat(assignments));

                var typeName = Parent.TypeDecl.Identifier.Text;
                return
                    SyntaxFactory.ConstructorDeclaration(typeName)
                    .WithModifiers(SyntaxTokenList.Create(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                    .WithParameterList(parameterList)
                    .WithBody(body)
                    .WithAdditionalAnnotations(Formatter.Annotation);
            }

            public ConstructorGenerator(RecordTypeGenerator parent,
                        ImmutableArray<AssignableVariableMember> assignables)
            {
                Parent = parent;
                Assignables = assignables;
            }
        }

        SemanticModel SemanticModel { get; }

        TypeDeclarationSyntax TypeDecl { get; }

        ITypeSymbol TypeSymbol { get; }

        CancellationToken CancellationToken { get; }

        LanguageVersion? LanguageVersion =>
            (TypeDecl.SyntaxTree.Options as CSharpParseOptions).LanguageVersion;

        ExpressionSyntax NameOfSyntax(SyntaxToken identifier)
        {
            if (LanguageVersion?.SupportsNameOf() == true)
            {
                return
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.IdentifierName("nameof"),
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.Argument(
                                    SyntaxFactory.IdentifierName(identifier)
                                ))));
            }

            return
                SyntaxFactory.LiteralExpression(
                    SyntaxKind.StringLiteralExpression,
                    SyntaxFactory.Literal(identifier.Text)
                );
        }

        ConstructorDeclarationSyntax GenerateRecordConstructor()
        {
            var varMembers = new VariableMemberCollector(SemanticModel).Collect(TypeDecl);

            var assignables =
                varMembers
                .Where(m => !m.HasInitializer)
                .Select(m => new AssignableVariableMember(m))
                .ToImmutableArray();

            return new ConstructorGenerator(this, assignables).Generate();
        }

        public async Task<Document> FixAsync(Document document)
        {
            var tree = await document.GetSyntaxTreeAsync(CancellationToken).ConfigureAwait(false);
            var root = await tree.GetRootAsync(CancellationToken);

            var constructorDecl = GenerateRecordConstructor();

            var newTypeDecl = TypeDecl.AddMembers(constructorDecl);

            return document.WithSyntaxRoot(root.ReplaceNode(TypeDecl, newTypeDecl));
        }

        public RecordTypeGenerator(
                SemanticModel semanticModel,
                TypeDeclarationSyntax typeDecl,
                ITypeSymbol typeSymbol,
                CancellationToken cancellationToken)
        {
            SemanticModel = semanticModel;
            TypeDecl = typeDecl;
            TypeSymbol = typeSymbol;
            CancellationToken = cancellationToken;
        }

        public static async Task Register(CodeFixContext context)
        {
            var diagnostic = context.Diagnostics.FirstOrDefault();
            if (diagnostic == null) return;

            var document = context.Document;
            var cancellationToken = context.CancellationToken;

            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            var diagnosticSpan = diagnostic.Location.SourceSpan;
            var typeDecl =
                root
                .FindToken(diagnosticSpan.Start)
                .Parent
                .AncestorsAndSelf()
                .OfType<TypeDeclarationSyntax>()
                .First();

            var semanticModel = await document.GetSemanticModelAsync(cancellationToken);

            var typeSymbol = semanticModel.GetDeclaredSymbol(typeDecl, cancellationToken);

            context.RegisterCodeFix(
                CodeAction.Create(
                    diagnostic.Descriptor.Title.ToString(),
                    ct => new RecordTypeGenerator(semanticModel, typeDecl, typeSymbol, ct).FixAsync(document),
                    equivalenceKey: diagnostic.Id
                ),
                diagnostic
            );
        }
    }

    public static class SyntaxNodeExtension
    {
        public static TypeDeclarationSyntax AddMembers(this TypeDeclarationSyntax typeDecl, params MemberDeclarationSyntax[] members)
        {
            switch (typeDecl)
            {
                case ClassDeclarationSyntax classDecl:
                    return classDecl.AddMembers(members);
                case StructDeclarationSyntax structDecl:
                    return structDecl.AddMembers(members);
                default:
                    throw new InvalidOperationException();
            }
        }
    }
}
