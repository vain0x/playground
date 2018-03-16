using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace Dyxi.CSharp.Util
{
    public class LambdaCommand : ICommand
    {
        /// <summary>
        /// Create a command that's always executable.
        /// </summary>
        /// <param name="execute"></param>
        public LambdaCommand(Action<object> execute)
            : this((param) => true, execute)
        {
        }

        public LambdaCommand(Predicate<object> canExecute, Action<object> execute)
        {
            _canExecute = canExecute;
            _execute = execute;
        }

        public bool CanExecute(object param)
        {
            return (_canExecute != null && _execute != null ? _canExecute(param) : false);
        }

        public void Execute(object param)
        {
            Debug.Assert(_execute != null);
            _execute(param);
        }

        public event EventHandler CanExecuteChanged;

        private Predicate<object> _canExecute;
        private Action<object> _execute;
    }
}
