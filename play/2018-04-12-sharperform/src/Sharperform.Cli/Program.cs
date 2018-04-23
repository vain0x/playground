using System;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Path = System.IO.Path;

namespace Sharperform.Cli
{
    sealed class Program
    {
        private async Task EntryPointAsync()
        {
            try
            {
                var solutionDir = Path.GetFullPath("../../examples/ex-ast");
                var projectPath = Path.Combine(solutionDir, @"Examples.Ast/Examples.Ast.csproj");

                var workspace = new Sharperform.Build.SharperformWorkspace()
                {
                    Logger = Console.Error,
                };

                workspace.AddProject(projectPath);
                var messages = await workspace.CollectAsync();

                foreach (var m in messages)
                {
                    Console.WriteLine(m);
                }
            }
            catch (Exception ex)
            {
                Debug.WriteLine(ex);
                throw;
            }
        }

        private static void Main(string[] args)
        {
            new Program().EntryPointAsync().GetAwaiter().GetResult();
        }
    }
}
