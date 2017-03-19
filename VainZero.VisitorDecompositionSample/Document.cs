using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.VisitorDecompositionSample
{
    public abstract class Document
    {
        public abstract string Layout();
        public abstract Document Nest(int i);
        public abstract Document Concat(Document other);

        #region Null
        sealed class NullDocument
            : Document
        {
            public override string Layout()
            {
                return string.Empty;
            }

            public override Document Nest(int i)
            {
                return this;
            }

            public override Document Concat(Document other)
            {
                return other;
            }
        }

        public static Document Null { get; } =
            new NullDocument();
        #endregion

        #region Line
        sealed class LineDocument
            : Document
        {
            int Depth { get; }
            Document Document { get; }

            public LineDocument(int depth, Document document)
            {
                Depth = depth;
                Document = document;
            }

            public override string Layout()
            {
                var sb = new StringBuilder();
                sb.AppendLine();
                for (var i = 0; i < Depth; i++)
                {
                    sb.Append(" ");
                }
                sb.Append(Document.Layout());
                return sb.ToString();
            }

            public override Document Nest(int i)
            {
                return new LineDocument(Depth + i, Document.Nest(i));
            }

            public override Document Concat(Document other)
            {
                return new LineDocument(Depth, Document.Concat(other));
            }
        }

        public static Document Line { get; } =
            new LineDocument(0, Null);
        #endregion

        #region Text
        sealed class TextDocument
            : Document
        {
            string String { get; }
            Document Document { get; }

            public TextDocument(string @string, Document document)
            {
                String = @string;
                Document = document;
            }

            public override string Layout()
            {
                return String + Document.Layout();
            }

            public override Document Nest(int i)
            {
                return new TextDocument(String, Document.Nest(i));
            }

            public override Document Concat(Document other)
            {
                return new TextDocument(String, Document.Concat(other));
            }
        }

        public static Document Text(string @string)
        {
            return new TextDocument(@string, Null);
        }
        #endregion

        public static Document Bracket(string start, Document innerDocument, string end)
        {
            return
                Text(start)
                .Concat(Line)
                .Concat(innerDocument)
                .Nest(2)
                .Concat(Line)
                .Concat(Text(end));
        }
    }
}
