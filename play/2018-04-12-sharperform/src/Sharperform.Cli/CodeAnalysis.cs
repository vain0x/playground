using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Formatting;
using Sharperform.Syntax;
using IO = System.IO;
using Microsoft.CodeAnalysis.Simplification;
using Microsoft.CodeAnalysis.Options;

namespace Sharperform.CodeAnalysis
{
    internal sealed class DeclCollector
    {
        Workspace Workspace { get; }
        Project Project { get; }
        Document Document { get; }
        SyntaxTree Tree { get; }
        SyntaxNode Root { get; }
        SemanticModel Model { get; }
        IO.TextWriter Logger { get; }

        public DeclCollector(Workspace workspace, Project project, Document document, SyntaxTree tree, SyntaxNode root, SemanticModel model, IO.TextWriter logger)
        {
            Workspace = workspace;
            Project = project;
            Document = document;
            Tree = tree;
            Root = root;
            Model = model;
            Logger = logger;
        }

        private bool TryAnalyzeTypeDeclWithDeriveAttribute(TypeDeclarationSyntax typeDecl, out ImmutableArray<string> result)
        {
            IEnumerable<ISymbol> AttributeSymbolCandidates(SymbolInfo attrSymInfo)
            {
                Logger.WriteLine($"symbol={attrSymInfo.Symbol} reason={attrSymInfo.CandidateReason} candidates={attrSymInfo.CandidateSymbols.JoinMap(", ", x => x.Name)}");

                // NOTE: [Derive] is just a candidate because of NotAnAttribute for some reason.
                // Although [DeriveAttribute] is ok.
                var candidates =
                    attrSymInfo.CandidateReason == CandidateReason.NotAnAttributeType
                        ? attrSymInfo.CandidateSymbols
                        : ImmutableArray<ISymbol>.Empty;
                return
                    attrSymInfo.Symbol != null
                        ? candidates.Prepend(attrSymInfo.Symbol)
                        : candidates;
            }

            /// <summary>
            /// Gets constant argument values of attribute from syntax tree.
            /// NOTE: Semantic model reports "no arguments" for some reasaon.
            /// <code>
            /// typeSym.GetAttributes()[0].ConstructorArguments
            /// </code>
            /// </summary>
            /// <param name="syntax"></param>
            /// <returns></returns>
            IEnumerable<T> ConstantArguments<T>(AttributeSyntax attr)
            {
                return attr.ArgumentList.Arguments
                    .SelectMany(arg => Model.GetConstantValue(arg.Expression))
                    .OfType<T>();
            }

            var typeSym = Model.GetDeclaredSymbol(typeDecl);

            var deriveNames =
                (
                    from attrList in typeDecl.AttributeLists
                    from attr in attrList.Attributes
                    let attrSymInfo = Model.GetSymbolInfo(attr)
                    from attrSym in AttributeSymbolCandidates(attrSymInfo)
                    where attrSym.ContainingSymbol.Name == "DeriveAttribute"
                    from argValue in ConstantArguments<string>(attr)
                    select argValue
                ).ToImmutableArray();

            result = deriveNames;
            return deriveNames.Length > 0;
        }

        public ImmutableArray<string> Collect()
        {
            var typeDecls =
                Root
                .DescendantNodesAndSelf()
                .OfType<TypeDeclarationSyntax>()
                .ToImmutableArray();

            var items = new List<(TypeDeclarationSyntax typeDecl, ImmutableArray<string> deriveNames)>();

            foreach (var typeDecl in typeDecls)
            {
                var typeSym = (ITypeSymbol)Model.GetDeclaredSymbol(typeDecl);
                if (!TryAnalyzeTypeDeclWithDeriveAttribute(typeDecl, out var deriveNames)) continue;

                items.Add((typeDecl, deriveNames));
            }

            if (items.Count == 0)
            {
                return ImmutableArray<string>.Empty;
            }

            // Render:

            var sf = new MySyntaxFactory(LanguageVersion.CSharp5);
            var partialTypeDecls = new List<MemberDeclarationSyntax>();
            foreach (var (typeDecl, deriveNames) in items)
            {
                Logger.WriteLine($"#![derive({string.Join(", ", deriveNames)})]");
                Logger.WriteLine($"class {typeDecl.Identifier.ToString()};");

                var varMembers = new VariableMemberCollector(Model).Collect(typeDecl);
                var ctor = sf.CompleteConstructor(Model, typeDecl, varMembers);

                var partialTypeDecl =
                    typeDecl
                    .WithLeadingTrivia(SyntaxFactory.TriviaList())
                    .WithTrailingTrivia(SyntaxFactory.TriviaList())
                    .WithAttributeLists(SyntaxFactory.List<AttributeListSyntax>())
                    .WithModifiers(SyntaxFactory.TokenList(
                        SyntaxFactory.Token(SyntaxKind.PublicKeyword),
                        SyntaxFactory.Token(SyntaxKind.PartialKeyword)
                    ))
                    .WithMembers(new SyntaxList<MemberDeclarationSyntax>(new[]
                    {
                        ctor,
                    }));

                partialTypeDecls.Add(partialTypeDecl);
            }

            var ns =
                SyntaxFactory.NamespaceDeclaration(SyntaxFactory.ParseName("Examples.Ast"))
                .WithUsings(SyntaxFactory.List<UsingDirectiveSyntax>(new[]
                {
                    SyntaxFactory.UsingDirective(SyntaxFactory.ParseName("System"))
                }))
                .WithMembers(SyntaxFactory.List(partialTypeDecls));

            var generatedModule =
                SyntaxFactory
                .CompilationUnit()
                .WithMembers(SyntaxFactory.List<MemberDeclarationSyntax>(new[] { ns }))
                .WithAdditionalAnnotations(
                    Formatter.Annotation,
                    Simplifier.Annotation
                );
            var formatted = Formatter.Format(generatedModule, Workspace);
            var generatedDoc = Project.AddDocument("Sharperform.g.cs", formatted);
            var simplified = Simplifier.ReduceAsync(generatedDoc).Result;
            var generatedCode = simplified.GetTextAsync().Result.ToString();
            Logger.WriteLine(generatedCode);

            // Logger.WriteLine(formatted.ToString());

            return ImmutableArray.Create(generatedCode);
        }

        public string Render()
        {
            /*
            var doc = typeSyms.Select(typeSym =>
            {
                return SyntaxFactory.NamespaceDeclaration(SyntaxFactory.ParseName(typeSym.ContainingNamespace.ToDisplayString()));
            }).ToArray();
            */
            return "";
        }
    }

    /// <summary>
    /// Represents a member (field or auto property)
    /// which stores a value in an instance.
    /// </summary>
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

        public string NameAsParameter()
        {
            var name = SymbolBase.Name;
            if (name.Length >= 1 && char.IsUpper(name[0]))
            {
                return char.ToLowerInvariant(name[0]).ToString() + name.Substring(1);
            }
            else if (name.Length >= 2 && name[0] == '_')
            {
                return name.Substring(1);
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

            void OnFieldDecl(FieldDeclarationSyntax fieldDecl)
            {
                foreach (var varDecl in fieldDecl.Declaration.Variables)
                {
                    var symbol = SemanticModel.GetDeclaredSymbol(varDecl) as IFieldSymbol;
                    if (symbol == null || symbol.IsStatic) continue;

                    varMembers.Add(new VariableMember.Field(symbol, fieldDecl, varDecl));
                }
            }

            void OnPropertyDecl(PropertyDeclarationSyntax propertyDecl)
            {
                if (propertyDecl.AccessorList == null) return;
                if (propertyDecl.AccessorList.Accessors.Any(a => a.Body != null)) return;

                var symbol = SemanticModel.GetDeclaredSymbol(propertyDecl) as IPropertySymbol;
                if (symbol == null || symbol.IsStatic || symbol.GetMethod == null) return;

                varMembers.Add(new VariableMember.Property(symbol, propertyDecl));
            }

            foreach (var member in typeDecl.Members)
            {
                switch (member)
                {
                    case FieldDeclarationSyntax fieldDecl:
                        OnFieldDecl(fieldDecl);
                        break;
                    case PropertyDeclarationSyntax propertyDecl:
                        OnPropertyDecl(propertyDecl);
                        break;
                    default:
                        break;
                }
            }

            return varMembers.ToImmutable();
        }
    }
}
