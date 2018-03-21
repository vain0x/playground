using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reactive;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using Optional;

namespace DotNetKit.Reactive.Commands
{
    /// <summary>
    /// コマンドを生成する機能を提供する。
    /// </summary>
    public static class RaisableCommand
    {
        /// <summary>
        /// コマンドの生成処理を担うオブジェクトを取得・設定する。
        /// </summary>
        public static RaisableCommandFactory Factory { get; set; }

        static RaisableCommand()
        {
            Factory = new RaisableCommandFactory(h => { }, h => { });
        }

        /// <summary>
        /// コマンドを生成する。
        /// </summary>
        public static IRaisableCommand<P> Create<P>(Action<P> execute, Func<P, bool> canExecute)
        {
            return Factory.Create<P>(execute, canExecute);
        }

        /// <summary>
        /// パラメーターを使用しないコマンドを生成する。
        /// </summary>
        public static IRaisableCommand<Unit> Create(Action execute, Func<bool> canExecute)
        {
            return Create<Unit>(_ => execute(), _ => canExecute());
        }
    }

    public sealed class RaisableCommandFactory
    {
        private sealed class RaisableCommandImpl<TParameter>
            : IRaisableCommand<TParameter>
        {
            private readonly Func<TParameter, bool> _canExecute;
            private readonly Action<TParameter> _execute;
            private readonly Action<EventHandler> _onSubscribed;
            private readonly Action<EventHandler> _onUnsubscribed;

            private event EventHandler _canExecuteChanged;
            public event EventHandler CanExecuteChanged
            {
                add
                {
                    _canExecuteChanged += value;
                    _onSubscribed(value);
                }
                remove
                {
                    _canExecuteChanged -= value;
                    _onUnsubscribed(value);
                }
            }

            public void RaiseCanExecuteChanged()
            {
                var h = _canExecuteChanged;
                if (h != null) h(this, EventArgs.Empty);
            }

            private static bool ParameterTypeIsUnit()
            {
                return typeof(TParameter) == typeof(Unit);
            }

            public bool CanExecute(TParameter parameter)
            {
                return _canExecute(parameter);
            }

            public bool CanExecute(object parameter)
            {
                if (ParameterTypeIsUnit())
                {
                    return CanExecute((TParameter)(object)Unit.Default);
                }
                else
                {
                    return parameter is TParameter && CanExecute((TParameter)parameter);
                }
            }

            public void Execute(TParameter parameter)
            {
                if (!CanExecute(parameter))
                {
                    throw new InvalidOperationException("コマンドを実行できません。");
                }

                _execute(parameter);
            }

            public void Execute(object parameter)
            {
                if (ParameterTypeIsUnit())
                {
                    Execute((TParameter)(object)Unit.Default);
                }
                else
                {
                    if (!(parameter is TParameter))
                        throw new ArgumentException("parameter");

                    Execute((TParameter)parameter);
                }
            }

            public RaisableCommandImpl(Func<TParameter, bool> canExecute, Action<TParameter> execute, Action<EventHandler> onSubscribed, Action<EventHandler> onUnsubscribed)
            {
                _canExecute = canExecute;
                _execute = execute;
                _onSubscribed = onSubscribed;
                _onUnsubscribed = onUnsubscribed;
            }
        }

        private readonly Action<EventHandler> _onSubscribed;
        private readonly Action<EventHandler> _onUnsubscribed;

        public IRaisableCommand<P> Create<P>(Action<P> execute, Func<P, bool> canExecute)
        {
            return new RaisableCommandImpl<P>(canExecute, execute, _onSubscribed, _onUnsubscribed);
        }

        public RaisableCommandFactory(Action<EventHandler> onSubscribed, Action<EventHandler> onUnsubscribed)
        {
            _onSubscribed = onSubscribed;
            _onUnsubscribed = onUnsubscribed;
        }
    }
}
