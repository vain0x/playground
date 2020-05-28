using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.VisitorDecompositionSample
{
    public class DocumentFromNode
    {
        public Document Document { get; }
        
        public DocumentFromNode(Node node, Document left, Document right)
        {
            var innerDocument =
                left.Concat(Document.Line).Concat(right);
            Document = Document.Bracket("(Node " + node.Value, innerDocument, ")");
        }
    }

    #region Sample: Internal visitor
    public sealed class InternalDecomposer
        : TreeDecomposer<Document, Document>
    {
        public Document Decompose(TreeVisitor<Document, Document> visitor, Tree tree)
        {
            return tree.Accept(visitor, this);
        }
    }

    public class DocumentFromTree1
        : TreeVisitor<Document, Document>
    {
        InternalDecomposer Decomposer { get; } =
            new InternalDecomposer();

        public override Document VisitLeaf()
        {
            return Document.Text("leaf");
        }

        public override Document VisitNode(Node node, Document left, Document right)
        {
            return new DocumentFromNode(node, left, right).Document;
        }

        public static Document Run(Tree tree)
        {
            var visitor = new DocumentFromTree1();
            return tree.Accept(visitor, visitor.Decomposer);
        }
    }
    #endregion

    #region Sample: External visitor
    public sealed class ExternalDecomposer
        : TreeDecomposer<Document, Tree>
    {
        public Tree Decompose(TreeVisitor<Document, Tree> visitor, Tree tree)
        {
            return tree;
        }
    }

    public class DocumentFromTree2
        : TreeVisitor<Document, Tree>
    {
        ExternalDecomposer Decomposer { get; } =
            new ExternalDecomposer();

        public override Document VisitLeaf()
        {
            return Document.Text("leaf");
        }

        public override Document VisitNode(Node node, Tree left, Tree right)
        {
            var leftDocument = left.Accept(this, Decomposer);
            var rightDocument = right.Accept(this, Decomposer);
            return new DocumentFromNode(node, leftDocument, rightDocument).Document;
        }

        public static Document Run(Tree tree)
        {
            var visitor = new DocumentFromTree2();
            return tree.Accept(visitor, visitor.Decomposer);
        }
    }
    #endregion

    #region Sample: Paramorphism visitor
    public sealed class TreeAndDocument
    {
        public Tree ParentOrNull { get; }
        public Document Document { get; }

        public TreeAndDocument(Tree parentOrNull, Document document)
        {
            ParentOrNull = parentOrNull;
            Document = document;
        }
    }

    public sealed class ParamorphismDecomposer
        : TreeDecomposer<Document, TreeAndDocument>
    {
        public Tree ParentOrNull { get; }

        public ParamorphismDecomposer(Tree parentOrNull)
        {
            ParentOrNull = parentOrNull;
        }

        public TreeAndDocument Decompose(TreeVisitor<Document, TreeAndDocument> visitor, Tree tree)
        {
            return
                new TreeAndDocument(
                    tree,
                    tree.Accept(visitor, new ParamorphismDecomposer(tree))
                );
        }
    }
    #endregion
}
