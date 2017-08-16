using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;

namespace BoilerplateConstructorGenerator
{
    public static class DiagnosticProvider
    {
        public static DiagnosticDescriptor CompleteConstructorGeneration { get; } =
            new DiagnosticDescriptor(
                "BcgCompleteConstructorGeneration",
                "Generate Complete Constructor",
                "Generate complete constructor for '{0}'.",
                "Refactoring",
                DiagnosticSeverity.Info,
                isEnabledByDefault: true
            );

        public static ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } =
            ImmutableArray.Create(CompleteConstructorGeneration);
    }
}
