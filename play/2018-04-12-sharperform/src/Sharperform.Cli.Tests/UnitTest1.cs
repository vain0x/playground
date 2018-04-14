using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Text;
using Xunit;
using C = Microsoft.CodeAnalysis;
using Path = System.IO.Path;

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
            com.
        }
    }
}
