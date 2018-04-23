using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading.Tasks;
using Buildalyzer.Workspaces;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Sharperform.CodeAnalysis;
using B = Buildalyzer;
using IO = System.IO;

namespace Sharperform.Build
{
    public sealed class SharperformWorkspace
    {
        B.AnalyzerManager AnalyzerManager { get; }
        Workspace Workspace { get; }

        public IO.TextWriter Logger { get; set; } = Console.Out;

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
}
