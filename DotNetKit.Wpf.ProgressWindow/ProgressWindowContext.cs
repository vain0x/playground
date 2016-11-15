using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;

namespace DotNetKit.Wpf
{
    public sealed class ProgressWindowContext
    {
        public Window Owner { get; set; }
        public string Title { get; set; }
        public object Body { get; set; }

        public Task Task { get; set; }
        public CancellationTokenSource CancellationTokenSource { get; set; }

        bool IsCancellationRequested
        {
            get
            {
                var cts = CancellationTokenSource;
                return cts != null && cts.IsCancellationRequested;
            }
        }

        public void Cancel()
        {
            var cts = CancellationTokenSource;
            if (cts != null)
            {
                cts.Cancel();
            }
        }

        public bool? Show()
        {
            var task = Task;
            if (task == null)
            {
                throw new InvalidOperationException("Expects ProgressWindowContext.Task != null.");
            }

            task.ContinueWith(_ =>
            {
                Cancel();
            });

            if (task.IsCompleted || IsCancellationRequested) return false;

            var window =
                new ProgressWindow(this)
                {
                    Owner = Owner,
                    Title = Title,
                };

            var cts = CancellationTokenSource;
            if (cts != null)
            {
                cts.Token.Register(() => window.Dispatcher.Invoke(window.Close));
            }
            return window.ShowDialog();
        }
    }
}
