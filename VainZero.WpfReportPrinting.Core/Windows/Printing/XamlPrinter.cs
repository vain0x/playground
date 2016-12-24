using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Printing;

namespace VainZero.Windows.Printing
{
    public sealed class XamlPrinter
    {
        PageContent CreatePageContent(object dataContext, Size Size)
        {
            var page = new FixedPage();
            var presenter =
                new ContentPresenter()
                {
                    Content = dataContext,
                    Width = Size.Width,
                    Height = Size.Height,
                };
            presenter.UpdateLayout();
            page.Children.Add(presenter);
            return new PageContent() { Child = page };
        }

        /// <summary>
        /// 複数ページに分割済みの (あるいは1ページだけの) ドキュメントを印刷する。
        /// </summary>
        /// <param name="dataContexts"></param>
        public void PrintPages(IEnumerable<object> dataContexts)
        {
            var area = default(PrintDocumentImageableArea);
            var writer = PrintQueue.CreateXpsDocumentWriter(ref area);
            var size = new Size(area.ExtentWidth, area.ExtentHeight);

            var document = new FixedDocument();
            foreach (var dataContext in dataContexts)
            {
                var pageContent = CreatePageContent(dataContext, size);
                document.Pages.Add(pageContent);
            }

            writer.Write(document);
        }
    }
}
