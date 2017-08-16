using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;

namespace BoilerplateConstructorGenerator
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(BoilerplateConstructorGeneratorCodeFixProvider)), Shared]
    public class BoilerplateConstructorGeneratorCodeFixProvider : CodeFixProvider
    {
        static readonly ImmutableArray<string> s_fixableDiagnosticIds =
            ImmutableArray.Create(DiagnosticProvider.CompleteConstructorGeneration.Id);

        public sealed override ImmutableArray<string> FixableDiagnosticIds =>
            s_fixableDiagnosticIds;

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            return CompleteConstructors.Creating.MyFixer.Register(context);
        }
    }
}
