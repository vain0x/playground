using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Printing;
using VainZero.Windows.Documents;

namespace VainZero.WpfReportPrinting.Demo.Printing
{
    public sealed class Printer
    {
        public void Print(IPaginatable paginatable, Size pageSize)
        {
            var document = paginatable.ToFixedDocument(pageSize);

            var printServer = new LocalPrintServer();
            var queue = printServer.DefaultPrintQueue;
            queue.DefaultPrintTicket.PageMediaSize =
                new PageMediaSize(pageSize.Width, pageSize.Height);

            var writer = PrintQueue.CreateXpsDocumentWriter(queue);
            writer.Write(document);
        }
    }
}
