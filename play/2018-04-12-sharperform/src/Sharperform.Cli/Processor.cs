using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading.Tasks;
using Buildalyzer.Workspaces;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using IO = System.IO;
using B = Buildalyzer;
using C = Microsoft.CodeAnalysis;
using S = Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Sharperform.Build
{
    public sealed class SharperformWorkspace
    {
        B.AnalyzerManager AnalyzerManager { get; }
        Workspace Workspace { get; }
        IO.TextWriter Logger { get; set; } = Console.Out;

        public SharperformWorkspace()
        {
            AnalyzerManager = new B.AnalyzerManager();
            Workspace = AnalyzerManager.GetWorkspace();
        }

        public void AddProject(string projectPath)
        {
            var projectAnalyzer = AnalyzerManager.GetProject(projectPath);
            projectAnalyzer.AddToWorkspace(Workspace, addProjectReferences: true);
        }

        public async Task<ImmutableArray<dynamic>> CollectAsync()
        {
            var all = ImmutableArray.CreateBuilder<dynamic>();
            var projects = Workspace.CurrentSolution.Projects.ToImmutableArray();

            Logger.WriteLine($"Solution {projects.Length}");

            foreach (var project in projects)
            {
                Logger.WriteLine($"\nProject: {project.Name}");

                foreach (var document in project.Documents)
                {
                    Logger.WriteLine($"\nDocument: {document.Name}\n");

                    var tree = await document.GetSyntaxTreeAsync();
                    var root = await document.GetSyntaxRootAsync();
                    var model = await document.GetSemanticModelAsync();
                    var items = new DeclCollector(Workspace, project, document, tree, root, model, Logger).Collect();
                    all.AddRange(items);
                }
            }

            return all.ToImmutable();
        }
    }

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

            var items = new List<(ITypeSymbol typeSym, ImmutableArray<string> deriveNames)>();

            foreach (var typeDecl in typeDecls)
            {
                var typeSym = (ITypeSymbol)Model.GetDeclaredSymbol(typeDecl);
                if (!TryAnalyzeTypeDeclWithDeriveAttribute(typeDecl, out var deriveNames)) continue;

                items.Add((typeSym, deriveNames));
            }

            foreach (var (typeSym, deriveNames) in items)
            {
                Logger.WriteLine($"#![derive({string.Join(", ", deriveNames)})]");
                Logger.WriteLine($"class {typeSym.Name};");
            }

            return ImmutableArray<string>.Empty;
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
}
