using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Reactive.Commands
{
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
}
