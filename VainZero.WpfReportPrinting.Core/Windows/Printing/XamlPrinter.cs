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
        PageContent CreatePageContent(object dataContext, Size size)
        {
            var page = new FixedPage();
            var presenter =
                new ContentPresenter()
                {
                    Content = dataContext,
                    Width = size.Width,
                    Height = size.Height,
                };
            presenter.UpdateLayout();
            page.Children.Add(presenter);
            page.Width = size.Width;
            page.Height = size.Height;
            return new PageContent() { Child = page };
        }

        FixedDocument CreateDocument(IEnumerable<object> dataContexts, Size pageSize)
        {
            var document = new FixedDocument();
            foreach (var dataContext in dataContexts)
            {
                var pageContent = CreatePageContent(dataContext, pageSize);
                document.Pages.Add(pageContent);
            }
            return document;
        }

        public bool OpensPrintDialog { get; set; }

        /// <summary>
        /// 複数ページに分割済みの (あるいは1ページだけの) ドキュメントを印刷する。
        /// </summary>
        /// <param name="dataContexts"></param>
        public void
            PrintPages(
                IEnumerable<object> dataContexts,
                PageMediaSize previewMediaSize
            )
        {
            if (OpensPrintDialog)
            {
                var printDialog = new PrintDialog();
                var result = printDialog.ShowDialog();
                if (!result.HasValue || !result.Value) return;

                var queue = printDialog.PrintQueue;
                var ticket = printDialog.PrintTicket;

                var mediaSize = ticket.PageMediaSize;
                var pageSize = new Size(mediaSize.Width.Value, mediaSize.Height.Value);
                var document = CreateDocument(dataContexts, pageSize);
                printDialog.PrintDocument(document.DocumentPaginator, "xaml report printing");
            }
            else
            {
                var mediaSize = previewMediaSize;
                var pageSize = new Size(mediaSize.Width.Value, mediaSize.Height.Value);
                var document = CreateDocument(dataContexts, pageSize);

                var printServer = new LocalPrintServer();
                var queue = printServer.DefaultPrintQueue;
                queue.DefaultPrintTicket.PageMediaSize = mediaSize;
                var writer = PrintQueue.CreateXpsDocumentWriter(queue);

                writer.Write(document);
            }
        }
    }
}
