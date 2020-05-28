using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;

namespace VainZero.Windows.Documents
{
    public static class PaginatableExtension
    {
        public static FixedDocument
            ToFixedDocument(this IPaginatable paginatable, Size pageSize)
        {
            var document = new FixedDocument();

            foreach (var content in paginatable.Paginate(pageSize))
            {
                var presenter =
                    new ContentPresenter()
                    {
                        Content = content,
                        Width = pageSize.Width,
                        Height = pageSize.Height,
                    };

                var page =
                    new FixedPage()
                    {
                        Width = pageSize.Width,
                        Height = pageSize.Height,
                    };
                page.Children.Add(presenter);

                // この3つを行わないと DataGrid がページ全体に展開せず、潰れた状態になる。
                // これらが実際に何をするかは余裕があったら調べたい。
                page.Measure(pageSize);
                page.Arrange(new Rect(new Point(0, 0), pageSize));
                page.UpdateLayout();

                var pageContent = new PageContent() { Child = page };
                document.Pages.Add(pageContent);
            }

            return document;
        }
    }
}
