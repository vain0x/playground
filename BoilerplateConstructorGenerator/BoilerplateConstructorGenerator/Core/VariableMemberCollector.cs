using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.Simplification;

namespace BoilerplateConstructorGenerator
{
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
