using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Text;
using Xunit;
using C = Microsoft.CodeAnalysis;
using S = Microsoft.CodeAnalysis.CSharp.Syntax;
using Path = System.IO.Path;
using System.Diagnostics;
using Xunit.Abstractions;
using System.Collections.Generic;

namespace Sharperform.Cli.Tests
{
    public sealed class UnitTest
    {
        [Fact]
        public void Ok()
        {
            Assert.Equal(3, 1 + 2);
        }
    }

    public sealed class CompileExampleAst
    {
        ITestOutputHelper Output { get; }

        public CompileExampleAst(ITestOutputHelper output)
        {
            Output = output;
        }

        [Fact]
        public async Task Build()
        {
            var solutionPath = @"/media/owner/data/repo/github.com/vain0/playground/play/2018-04-12-sharperform/examples/ex-ast";
            var solutionFilePath = Path.Combine(solutionPath, @"ex-ast.sln");
            var projectPath = Path.Combine(solutionPath, @"Examples.Ast/Examples.Ast.csproj");

            var solutionInfo = C.SolutionInfo.Create(SolutionId.CreateNewId("ex-ast"), VersionStamp.Create(), solutionFilePath);

            var workspace = new AdhocWorkspace();
            workspace.AddSolution(solutionInfo);

            var projectId = ProjectId.CreateNewId();
            workspace.AddProject(ProjectInfo.Create(projectId, VersionStamp.Create(), "Examples.Ast", "Examples.Ast", LanguageNames.CSharp));

            workspace.TryApplyChanges(
                workspace.CurrentSolution.AddDocument(
                    DocumentId.CreateNewId(projectId, "UnitTest1"),
                    "UnitTest1",
                    System.IO.File.ReadAllText(
                        Path.Combine(solutionPath, "Examples.Ast/UnitTest1.cs")
                    )));

            var project = workspace.CurrentSolution.GetProject(projectId);

            var com = await project.GetCompilationAsync();
            var trees = com.SyntaxTrees;

            // CSharpCompilation.Create("ex-ast");
            Assert.Equal(1, trees.Count());

            var t = trees.First();
            var model = com.GetSemanticModel(t);

            var root = t.GetRoot();
            foreach (var node in root.DescendantNodesAndSelf())
            {
                Output.WriteLine($"t={node.GetType().Name}");
            }

            var typeDecls = root.DescendantNodesAndSelf().OfType<S.TypeDeclarationSyntax>().ToArray();
            Assert.Equal(3, typeDecls.Length);

            var typeSyms = new List<ITypeSymbol>();

            foreach (var typeDecl in typeDecls)
            {
                var typeSym = model.GetDeclaredSymbol(typeDecl);

                foreach (var attr in typeDecl.AttributeLists.SelectMany(a => a.Attributes))
                {
                    // なぜか NotAnAttribute といわれる。
                    var attrSymInfo = model.GetSymbolInfo(attr);
                    var attrSym = attrSymInfo.Symbol;
                    if (attrSym == null)
                    {
                        if (attrSymInfo.CandidateSymbols.Length != 1)
                        {
                            if (attrSymInfo.CandidateReason == CandidateReason.None) continue;

                            throw new Exception($"No candidate {attrSymInfo.CandidateReason}.");
                        }
                        attrSym = attrSymInfo.CandidateSymbols[0];
                    }

                    Output.WriteLine($"attrSym ={attrSym.ContainingSymbol.Name} ns={attrSym.ContainingNamespace.ToDisplayString()}");
                    if (attrSym.Name != "DeriveAttribute") continue;

                    var args = attr.ArgumentList.Arguments;
                    if (args.Count == 0) continue;

                    if (!(model.GetConstantValue(args[0].Expression).Value is string deriveType)) continue;
                    Output.WriteLine(deriveType);
                }

                typeSyms.Add(typeSym);

                foreach (var attr in typeSym.GetAttributes())
                {
                    Output.WriteLine(attr.AttributeClass.Name);
                    Output.WriteLine(attr.ConstructorArguments.Count().ToString());
                    Output.WriteLine("attr args = " + string.Join(", ", attr.ConstructorArguments.Select(a => a.ToCSharpString())));
                    Output.WriteLine(
                        attr.NamedArguments.Length.ToString()
                    );
                }

                void WriteSymbolInfo(SymbolInfo info)
                {
                    if (info.Symbol != null)
                    {
                        Output.WriteLine(info.Symbol.ToDisplayString());
                    }
                    else
                    {
                        var candidates = string.Join(
                        ",",
                        info.CandidateSymbols.Select(
                            sym => sym.ToDisplayString()
                        )
                    );
                        Output.WriteLine($"R={info.CandidateReason} C={candidates}");
                    }
                }

                foreach (var attr in typeDecl.AttributeLists.SelectMany(a => a.Attributes))
                {
                    foreach (var arg in attr.ArgumentList.Arguments)
                    {
                        var symInfo = model.GetSymbolInfo(arg.Expression);
                        WriteSymbolInfo(symInfo);
                    }
                }
            }


            // Render.

            var doc = typeSyms.Select(typeSym =>
            {
                return SyntaxFactory.NamespaceDeclaration(SyntaxFactory.ParseName(typeSym.ContainingNamespace.ToDisplayString()));
            }).ToArray();

        }
    }
}
