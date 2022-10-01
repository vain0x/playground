using System;
using System.Windows.Input;

namespace AppDesktop
{
    internal sealed class Command<T> : ICommand
    {
        private readonly Func<T, bool>? canExecuteFunc;
        private readonly Action<T> execute;

        public Command(Func<T, bool>? canExecuteFunc, Action<T> execute)
        {
            this.canExecuteFunc = canExecuteFunc;
            this.execute = execute;
        }

        public event EventHandler? CanExecuteChanged;

        public bool CanExecute(T parameter) => canExecuteFunc == null || canExecuteFunc(parameter);

        public void Execute(T parameter)
        {
            if (canExecuteFunc == null || canExecuteFunc(parameter))
            {
                execute(parameter);
            }
        }

        public void RaiseCanExecuteChanged()
        {
            CanExecuteChanged?.Invoke(this, EventArgs.Empty);
        }

        bool ICommand.CanExecute(object? parameter) => CanExecute((T)parameter!);

        void ICommand.Execute(object? parameter)
        {
            Execute((T)parameter!);
        }
    }

    internal static class Command
    {
        public static Command<T> Create<T>(Action<T> action) => new(null, action);

        public static Command<T> CreateWithCanExecute<T>(Func<T, bool> canExecute, Action<T> action) => new(canExecute, action);
    }

    internal sealed class EventCommand<T> : ICommand
    {
        private readonly object? sender;
        private readonly Func<T, bool>? canExecuteFunc;

        public event EventHandler<T>? Executed;

        public EventCommand(object? sender, Func<T, bool>? canExecuteFunc)
        {
            this.sender = sender;
            this.canExecuteFunc = canExecuteFunc;
        }

        public event EventHandler? CanExecuteChanged;

        public bool CanExecute(T parameter) => canExecuteFunc == null || canExecuteFunc(parameter);

        public void Execute(T parameter)
        {
            if (canExecuteFunc == null || canExecuteFunc(parameter))
            {
                Executed?.Invoke(sender, parameter);
            }
        }

        public void RaiseCanExecuteChanged()
        {
            CanExecuteChanged?.Invoke(this, EventArgs.Empty);
        }

        bool ICommand.CanExecute(object? parameter) => CanExecute((T)parameter!);

        void ICommand.Execute(object? parameter)
        {
            Execute((T)parameter!);
        }
    }

    internal static class EventCommand
    {
        public static EventCommand<T> Create<T>(object? sender) => new(sender, null);

        public static EventCommand<T> CreateWithCanExecute<T>(object? sender, Func<T, bool> canExecute) => new(sender, canExecute);
    }
}
