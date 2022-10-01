using System;
using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace AppDesktop
{
    internal abstract class BindableBase : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler? PropertyChanged;

        protected void RaisePropertyChanged([CallerMemberName] string? name = null)
        {
            if (name == null) throw new ArgumentNullException(nameof(name));
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(name));
        }
    }
}
