using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;

namespace Dyxi.CSharp.Util
{
    public class ViewModelBase : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        public void RaisePropertyChanged([CallerMemberName] string propertyName = null)
        {
            Debug.Assert(propertyName != null);
            if (PropertyChanged == null) return;
            PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
        }
    }

    public class DialogViewModelBase<T> : ViewModelBase
        where T : class
    {
        public Visibility Visibility
        {
            get { return _data == null ? Visibility.Collapsed : Visibility.Visible; }
        }

        public T Data
        {
            get { return _data; }
            set
            {
                if (_data != value)
                {
                    _data = value;
                    RaisePropertyChanged("Visibility");
                }
            }
        }

        public void Show(T x)
        {
            if (x == null) throw new ArgumentNullException();
            Data = x;
        }

        public bool Hide()
        {
            if (Data == null)
            {
                return false;
            }
            else
            {
                Data = null;
                return true;
            }
        }

        public void OnClosing(CancelEventArgs e)
        {
            e.Cancel = Hide();
        }

        private T _data = null;
    }
}
