using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace DotNetKit.Functional.Commands
{
    sealed class AlwaysExecutableCommand
        : ICommand
    {
        Action execute;

        public event EventHandler CanExecuteChanged
        {
            add { }
            remove { }
        }

        public bool CanExecute(object parameter)
        {
            return true;
        }

        public void Execute(object parameter)
        {
            execute();
        }

        public AlwaysExecutableCommand(Action execute)
        {
            this.execute = execute;
        }

        public static AlwaysExecutableCommand Empty { get; } =
            new AlwaysExecutableCommand(() => { });
    }
}
