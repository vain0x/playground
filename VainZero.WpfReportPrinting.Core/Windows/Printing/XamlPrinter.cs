using System.Collections.Generic;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Printing;

namespace VainZero.Windows.Printing
{
    public abstract class XamlPrinter
    {
        public abstract void Print(IEnumerable<object> dataContexts);
    }

    public sealed class ConcreteXamlPrinter
        : XamlPrinter
    {
        public override void Print(IEnumerable<object> dataContexts)
        {
            var area = default(PrintDocumentImageableArea);
            var writer = PrintQueue.CreateXpsDocumentWriter(ref area);

            var document = new FixedDocument();

            foreach (var dataContext in dataContexts)
            {
                var page = new FixedPage();
                var presenter =
                    new ContentPresenter()
                    {
                        Content = dataContext,
                        Width = area.ExtentWidth,
                        Height = area.ExtentHeight,
                    };
                presenter.UpdateLayout();
                page.Children.Add(presenter);
                document.Pages.Add(new PageContent() { Child = page });
            }

            writer.Write(document);
        }
    }
}
