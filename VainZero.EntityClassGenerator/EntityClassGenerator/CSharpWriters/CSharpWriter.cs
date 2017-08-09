using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VainZero.Misc.Disposables;

namespace VainZero.EntityClassGenerator.CSharpWriters
{
    public abstract class CSharpWriter
    {
        protected TextWriter Writer { get; }

        public struct Indentation
            : IDisposable
        {
            CSharpWriter Parent { get; }

            public Indentation(CSharpWriter parent)
            {
                Parent = parent;

                Parent.indentLevel++;
            }

            public void Dispose()
            {
                Parent.indentLevel--;
            }
        }

        static IDisposable Defer(Action action)
        {
            return new AnonymousDisposable(action);
        }

        public static string EscapeIdentifier(string name)
        {
            return new CSharpIdentifier(name).Name;
        }

        public Indentation AddIndent()
        {
            return new Indentation(this);
        }

        int indentLevel;

        public void WriteIndent()
        {
            for (var i = 0; i < indentLevel * 4; i++)
            {
                Writer.Write(' ');
            }
        }

        public void WriteLine(string line)
        {
            if (string.IsNullOrWhiteSpace(line))
            {
                Writer.WriteLine();
                return;
            }

            WriteIndent();
            Writer.WriteLine(line);
        }

        public IDisposable WriteBlock()
        {
            WriteLine("{");
            var indentation = AddIndent();
            return
                Defer(() =>
                {
                    indentation.Dispose();
                    WriteLine("}");
                });
        }

        public IDisposable WriteNamespaceBlock(string namespacePath)
        {
            WriteLine($"namespace {namespacePath}");
            return WriteBlock();
        }

        public CSharpWriter(TextWriter writer)
        {
            Writer = writer;
        }
    }
}
