using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using DotNetKit.Functional.Commands;

namespace DotNetKit.Windows.Controls
{
    public abstract class ToastNotification
    {
        public event EventHandler Removed;
        public ICommand RemoveCommand { get; }

        public abstract Duration FadeDuration { get; }

        public void Remove()
        {
            RemoveCommand.Execute(null);
        }

        public ToastNotification()
        {
            RemoveCommand =
                new AlwaysExecutableCommand(() => Removed?.Invoke(this, EventArgs.Empty));
        }
    }
}
