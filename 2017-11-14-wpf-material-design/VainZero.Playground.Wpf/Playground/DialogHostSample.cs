using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using Prism.Commands;
using Prism.Mvvm;

namespace VainZero.Playground
{
    public sealed class DialogHostSample
        : BindableBase
    {
        IDialog _dialog;
        public IDialog Dialog
        {
            get => _dialog;
            set => SetProperty(ref _dialog, value);
        }

        public ICommand OpenCommand { get; }
        public ICommand CloseCommand { get; }

        public DialogHostSample()
        {
            OpenCommand = new DelegateCommand(() =>
            {
                Dialog = new HelloDialog();
            });

            CloseCommand = new DelegateCommand(() =>
            {
                Dialog = null;
            });
        }
    }

    public interface IDialog
    {
        void OnOpened();
        void OnClosing(Action cancel);
        void OnClosed();
    }

    public sealed class HelloDialog
        : IDialog
    {
        public void OnOpened()
        {
            Debug.WriteLine("Opened.");
        }

        public void OnClosing(Action cancel)
        {
            Debug.WriteLine("Closing.");
        }

        public void OnClosed()
        {
            Debug.WriteLine("Closed.");
        }
    }
}
