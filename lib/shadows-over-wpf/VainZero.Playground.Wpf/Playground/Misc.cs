using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace VainZero.Playground
{
    public static class MiscExtension
    {
        public static bool EqualsGeneric<X>(this X @this, X other)
        {
            return EqualityComparer<X>.Default.Equals(@this, other);
        }
    }

    /// <summary>
    /// Represents meaningless value.
    /// This type is useful as a type argument.
    /// </summary>
    public struct Unit
        : IEquatable<Unit>
        , IComparable
        , IComparable<Unit>
    {
        /// <summary>
        /// Gets the unit value.
        /// You can also use <c>default(Unit)</c>.
        /// </summary>
        public static Unit Instance => default(Unit);

        public static Unit Default => Instance;

        #region Equality
        /// <summary>
        /// Gets a value indicating whether two unit values are equal. Always <c>true</c>.
        /// </summary>
        public bool Equals(Unit other)
        {
            return true;
        }

        /// <summary>
        /// Gets a value indicating whether the specified value is the unit value.
        /// </summary>
        public override bool Equals(object obj)
        {
            return obj is Unit;
        }

        /// <summary>
        /// Serves a hash code.
        /// </summary>
        public override int GetHashCode()
        {
            return 1;
        }

        /// <summary>
        /// Gets a value indicating whether two unit values are equal. Always <c>true</c>.
        /// </summary>
        public static bool operator ==(Unit left, Unit right)
        {
            return true;
        }

        /// <summary>
        /// Gets a value indicating whether two unit values are not equal. Always <c>false</c>.
        /// </summary>
        public static bool operator !=(Unit left, Unit right)
        {
            return false;
        }
        #endregion

        #region Comparison
        /// <summary>
        /// Compares a value to the unit value.
        /// Throws an exception if the value isn't unit.
        /// </summary>
        /// <exception cref="InvalidOperationException" />
        public int CompareTo(object obj)
        {
            if (obj is Unit) return 0;
            throw new InvalidOperationException("Unit can't be compared to non-unit values.");
        }

        /// <summary>
        /// Compares two unit values. Always equal.
        /// </summary>
        public int CompareTo(Unit other)
        {
            return 0;
        }
        #endregion
    }

    /// <summary>
    /// コマンドを表す。(<see cref="ICommand"/> の型つき版)
    /// </summary>
    public interface ICommand<in TParameter>
        : ICommand
    {
        /// <summary>
        /// 指定されたパラメーターについて、コマンドが実行可能かを取得する。
        /// </summary>
        bool CanExecute(TParameter parameter);

        /// <summary>
        /// 指定されたパラメーターについて、コマンドを実行する。
        /// </summary>
        void Execute(TParameter parameter);
    }

    /// <summary>
    /// コマンドを表す。
    /// </summary>
    public interface IRaisableCommand<in TParameter>
        : ICommand<TParameter>
    {
        /// <summary>
        /// コマンドが実行可能かどうかの条件が変化した可能性があることを通知する。
        /// </summary>
        void RaiseCanExecuteChanged();
    }

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
            Factory = new RaisableCommandFactory(
                h => CommandManager.RequerySuggested += h,
                h => CommandManager.RequerySuggested -= h
            );
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
