using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;

namespace RecordTypeAnalyzer.Analyzers
{
    public static class DiagnosticProvider
    {
        public static DiagnosticDescriptor ImplGeneration { get; } =
            new DiagnosticDescriptor(
                "RtaGen",
                "Generate Record-like Implementation",
                "You can generate record-like implementation to the type '{0}'.",
                "Generating",
                DiagnosticSeverity.Info,
                isEnabledByDefault: true
            );

        public static ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } =
            ImmutableArray.Create(ImplGeneration);
    }
}
