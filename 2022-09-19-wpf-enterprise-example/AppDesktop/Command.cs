using System;
using System.Windows.Input;

namespace AppDesktop
{
    internal sealed class Command<T> : ICommand
    {
        private Func<T, bool>? canExecuteFunc;
        private Action<T> execute;

        public Command(Func<T, bool>? canExecuteFunc, Action<T> execute)
        {
            this.canExecuteFunc = canExecuteFunc;
            this.execute = execute;
        }

        public event EventHandler? CanExecuteChanged;

        public void RaiseCanExecuteChanged()
        {
            CanExecuteChanged?.Invoke(this, EventArgs.Empty);
        }

        bool ICommand.CanExecute(object? parameter) =>
            canExecuteFunc == null
            || canExecuteFunc((T)parameter!);

        void ICommand.Execute(object? parameter) =>
            execute((T)parameter!);
    }

    internal static class Command
    {
        public static Command<T> Create<T>(Action<T> action) => new(null, action);

        public static Command<T> CreateWithCanExecute<T>(Func<T, bool> canExecute, Action<T> action) => new(canExecute, action);
    }
}
