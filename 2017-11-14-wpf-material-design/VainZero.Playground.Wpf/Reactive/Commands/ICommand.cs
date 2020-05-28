using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace DotNetKit.Reactive.Commands
{
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
}
