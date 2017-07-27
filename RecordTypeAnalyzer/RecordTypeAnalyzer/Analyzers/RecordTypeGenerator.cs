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

                                varMembers.Add(new VariableMember.Field(symbol, fieldDecl, varDecl));
                            }
                            break;
                        }
                    case PropertyDeclarationSyntax propertyDecl:
                        {
                            if (propertyDecl.AccessorList == null) continue;
                            if (propertyDecl.AccessorList.Accessors.Any(a => a.Body != null)) continue;

                            var symbol = SemanticModel.GetDeclaredSymbol(propertyDecl) as IPropertySymbol;
                            if (symbol == null || symbol.GetMethod == null) continue;

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
            VariableMember Member { get; }

            public string MemberName => Member.SymbolBase.Name;

            public string ParameterName { get; }

            public ITypeSymbol TypeSymbol => Member.TypeSymbol;

            public AssignableVariableMember(VariableMember member)
            {
                Member = member;
                ParameterName = member.NameAsCamelCase();
            }
        }

        SemanticModel SemanticModel { get; }

        TypeDeclarationSyntax TypeDecl { get; }

        ITypeSymbol TypeSymbol { get; }

        CancellationToken CancellationToken { get; }

        LanguageVersion? LanguageVersion =>
            (TypeDecl.SyntaxTree.Options as CSharpParseOptions).LanguageVersion;

        TypeSyntax TypeSyntax(ITypeSymbol typeSymbol)
        {
            var typeName = typeSymbol.ToDisplayString();
            return SyntaxFactory.IdentifierName(typeName);
        }

        MemberAccessExpressionSyntax ParameterNode(string parameterName)
        {
            return
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.ThisExpression(),
                    SyntaxFactory.IdentifierName(parameterName)
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

            var parameterList =
                SyntaxFactory.ParameterList(
                    SyntaxFactory.SeparatedList(
                        assignables.Select(a =>
                            SyntaxFactory.Parameter(
                                SyntaxFactory.Identifier(a.ParameterName)
                            ).WithType(TypeSyntax(a.TypeSymbol))
                        )));

            var aneType = SemanticModel.Compilation.GetTypeByMetadataName(typeof(ArgumentNullException).FullName);

            var contracts =
                (aneType == null ? ImmutableArray<AssignableVariableMember>.Empty : assignables)
                .Where(a => a.TypeSymbol.IsReferenceType)
                .Select(a =>
                    SyntaxFactory.IfStatement(
                        SyntaxFactory.BinaryExpression(
                            SyntaxKind.NotEqualsExpression,
                            ParameterNode(a.ParameterName),
                            SyntaxFactory.LiteralExpression(SyntaxKind.NullKeyword)
                        ),
                        SyntaxFactory.ThrowStatement(
                            SyntaxFactory.ObjectCreationExpression(TypeSyntax(aneType))
                        )));

            var assignments =
                assignables.Select(a =>
                    SyntaxFactory.ExpressionStatement(
                        SyntaxFactory.AssignmentExpression(
                            SyntaxKind.SimpleAssignmentExpression,
                            SyntaxFactory.MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                SyntaxFactory.ThisExpression(),
                                SyntaxFactory.IdentifierName(a.ParameterName)
                            ),
                            SyntaxFactory.IdentifierName(a.ParameterName)
                        )));

            var body = SyntaxFactory.Block(contracts.Concat<StatementSyntax>(assignments));

            var typeName = TypeDecl.Identifier.Text;
            return
                SyntaxFactory.ConstructorDeclaration(typeName)
                .WithModifiers(SyntaxTokenList.Create(SyntaxFactory.Token(SyntaxKind.PublicKeyword)))
                .WithParameterList(parameterList)
                .WithBody(body)
                .WithAdditionalAnnotations(Formatter.Annotation)
                .WithAdditionalAnnotations(Simplifier.Annotation);
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
                    diagnostic.GetMessage(),
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
