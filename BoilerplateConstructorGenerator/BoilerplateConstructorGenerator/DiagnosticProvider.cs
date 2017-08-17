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

        public static DiagnosticDescriptor CompleteConstructorFix { get; } =
            new DiagnosticDescriptor(
                "BcgCompleteConstructorFix",
                "Fix Complete Constructor",
                "Fix complete constructor of '{0}'.",
                "Refactoring",
                DiagnosticSeverity.Warning,
                isEnabledByDefault: true
            );
    }
}
