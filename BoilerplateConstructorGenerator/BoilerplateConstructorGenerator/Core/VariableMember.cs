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
    /// <summary>
    /// Represents a member (field or auto property)
    /// which stores a value in an instance.
    /// </summary>
    public abstract class VariableMember
    {
        VariableMember()
        {
        }

        public abstract ISymbol SymbolBase { get; }

        public abstract SyntaxNode DeclBase { get; }

        public abstract ITypeSymbol TypeSymbol { get; }

        public abstract bool IsReadOnly { get; }

        public abstract bool HasInitializer { get; }

        public string NameAsParameter()
        {
            var name = SymbolBase.Name;
            if (name.Length >= 1 && char.IsUpper(name[0]))
            {
                return char.ToLowerInvariant(name[0]).ToString() + name.Substring(1);
            }
            else if (name.Length >= 2 && name[0] == '_')
            {
                return name.Substring(1);
            }
            return name;
        }

        public sealed class Field
            : VariableMember
        {
            public IFieldSymbol Symbol { get; }

            public FieldDeclarationSyntax FieldDecl { get; }

            public VariableDeclaratorSyntax VarDecl { get; }

            public override ISymbol SymbolBase => Symbol;

            public override SyntaxNode DeclBase => VarDecl;

            public Field(IFieldSymbol symbol, FieldDeclarationSyntax fieldDecl, VariableDeclaratorSyntax varDecl)
            {
                Symbol = symbol;
                FieldDecl = fieldDecl;
                VarDecl = varDecl;
            }

            public override ITypeSymbol TypeSymbol =>
                Symbol.Type;

            public override bool IsReadOnly =>
                Symbol.IsReadOnly;

            public override bool HasInitializer =>
                VarDecl.Initializer != null;
        }

        public sealed class Property
            : VariableMember
        {
            public IPropertySymbol Symbol { get; }

            public PropertyDeclarationSyntax Decl { get; }

            public override ISymbol SymbolBase => Symbol;

            public override SyntaxNode DeclBase => Decl;

            public Property(IPropertySymbol symbol, PropertyDeclarationSyntax decl)
            {
                Symbol = symbol;
                Decl = decl;
            }

            public override ITypeSymbol TypeSymbol =>
                Symbol.Type;

            public override bool IsReadOnly =>
                Symbol.SetMethod == null
                || (Symbol.DeclaredAccessibility != Accessibility.Private
                    && Symbol.SetMethod.DeclaredAccessibility == Accessibility.Private);

            public override bool HasInitializer =>
                Decl.Initializer != null;
        }
    }
}
