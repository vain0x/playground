using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;

namespace DotNetKit.Wpf
{
    public sealed class ProgressWindowContext
        : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            var h = PropertyChanged;
            if (h != null) h.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        public Window Owner { get; set; }

        string title;
        public string Title
        {
            get { return title; }
            set
            {
                title = value;
                OnPropertyChanged();
            }
        }

        object content;
        public object Content
        {
            get { return content; }
            set
            {
                content = value;
                OnPropertyChanged();
            }
        }

        object cancelButtonContent = "Cancel";
        public object CancelButtonContent
        {
            get { return cancelButtonContent; }
            set
            {
                cancelButtonContent = value;
                OnPropertyChanged();
            }
        }

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
