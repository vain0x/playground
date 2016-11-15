using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;

namespace DotNetKit.Wpf
{
    /// <summary>
    /// Provies a configuration for a progress window.
    /// <para lang="ja">
    /// プログレスウィンドウの設定を提供する。
    /// </para>
    /// </summary>
    public sealed class ProgressWindowContext
        : INotifyPropertyChanged
    {
        /// <summary>
        /// Occurs whenever one of properties changes.
        /// </summary>
        public event PropertyChangedEventHandler PropertyChanged;

        void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            var h = PropertyChanged;
            if (h != null) h.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        /// <summary>
        /// Gets or sets the owner window for the progress window.
        /// <para lang="ja">
        /// プログレスウィンドウの親となるウィンドウを取得・設定する。
        /// </para>
        /// </summary>
        public Window Owner { get; set; }

        string title;

        /// <summary>
        /// Gets or sets the title bar caption of the progress window.
        /// <para lang="ja">
        /// プログレスウィンドウのタイトルバーキャプションを取得・設定する。
        /// </para>
        /// </summary>
        public string Title
        {
            get { return title; }
            set
            {
                title = value;
                OnPropertyChanged();
            }
        }

        object content;

        /// <summary>
        /// Gets or sets the content the progress window displays.
        /// You can assign a string.
        /// <para lang="ja">
        /// プログレスウィンドウに表示する内容を取得・設定する。
        /// 通常は、文字列を設定する。
        /// </para>
        /// </summary>
        public object Content
        {
            get { return content; }
            set
            {
                content = value;
                OnPropertyChanged();
            }
        }

        object cancelButtonContent = "Cancel";

        /// <summary>
        /// Gets or sets the content the cancel button contains.
        /// "Cancel" (string) by default.
        /// <para lang="ja">
        /// キャンセルボタンに表示する内容を取得・設定する。
        /// 既定値は「Cancel」。
        /// </para>
        /// </summary>
        public object CancelButtonContent
        {
            get { return cancelButtonContent; }
            set
            {
                cancelButtonContent = value;
                OnPropertyChanged();
            }
        }

        double progressRateValue;

        /// <summary>
        /// Gets the current progress rate as a percentage
        /// or throws an exception if <see cref="IsIndeterminate"/> is true.
        /// <para lang="ja">
        /// 現在の進捗の割合を百分率で取得する。
        /// ただし、<see cref="IsIndeterminate"/> が true の場合は例外を送出する。
        /// </para>
        /// </summary>
        public double ProgressRateValue
        {
            get
            {
                if (IsIndeterminate) throw new InvalidOperationException();
                return progressRateValue;
            }
            private set
            {
                progressRateValue = value;
                OnPropertyChanged();
            }
        }

        bool isIndeterminate = true;

        /// <summary>
        /// Gets a value indicating whether the progress rate is indeterminate.
        /// <para lang="ja">
        /// これが進捗率が不定であるかを取得する。
        /// </para>
        /// </summary>
        public bool IsIndeterminate
        {
            get { return isIndeterminate; }
            private set
            {
                isIndeterminate = value;
                OnPropertyChanged();
            }
        }

        /// <summary>
        /// Gets or sets the progress rate as a percentage
        /// or <c>null</c> if the rate is indeterminate.
        /// <para lang="ja">
        /// 進捗率を百分率として取得・設定する。
        /// 進捗率が不定の場合は、<c>null</c> を取得・設定する。
        /// </para>
        /// </summary>
        public double? ProgressRate
        {
            get { return IsIndeterminate ? default(double?) : ProgressRateValue; }
            set
            {
                if (value.HasValue)
                {
                    ProgressRateValue = value.Value;
                    IsIndeterminate = false;
                }
                else
                {
                    IsIndeterminate = true;
                }
            }
        }

        /// <summary>
        /// Gets or sets the task.
        /// <para lang="ja">
        /// タスクを取得・設定する。
        /// </para>
        /// </summary>
        public Task Task { get; set; }

        /// <summary>
        /// Gets or sets an object to cancel the task.
        /// <para lang="ja">
        /// タスクのキャンセルを行うためのオブジェクトを取得・設定する。
        /// </para>
        /// </summary>
        public CancellationTokenSource CancellationTokenSource { get; set; }

        bool IsCancellationRequested
        {
            get
            {
                var cts = CancellationTokenSource;
                return cts != null && cts.IsCancellationRequested;
            }
        }

        /// <summary>
        /// Requests to cancel the task.
        /// <para lang="ja">
        /// タスクのキャンセルを要求する。
        /// </para>
        /// </summary>
        public void Cancel()
        {
            var cts = CancellationTokenSource;
            if (cts != null)
            {
                cts.Cancel();
            }
        }

        ProgressWindow progressWindow;

        void Close()
        {
            var window = progressWindow;
            if (window != null)
            {
                progressWindow = null;
                window.Dispatcher.Invoke(window.Close);
            }
        }

        /// <summary>
        /// Shows a progress window as a modal dialog.
        /// You must set <see cref="Task"/> property before invoking this method.
        /// <para lang="ja">
        /// プログレスウィンドウをモーダルダイアログとして表示する。
        /// このメソッドを起動する前に、<see cref="Task"/> プロパティに値を設定する必要がある。
        /// </para>
        /// </summary>
        /// <returns></returns>
        public bool? ShowDialog()
        {
            var task = Task;
            if (task == null)
            {
                throw new InvalidOperationException("Expects ProgressWindowContext.Task != null.");
            }

            task.ContinueWith(_ =>
            {
                Cancel();
                Close();
            });

            if (task.IsCompleted || IsCancellationRequested) return false;

            progressWindow = 
                new ProgressWindow(this)
                {
                    Owner = Owner,
                };

            var cts = CancellationTokenSource;
            if (cts != null)
            {
                cts.Token.Register(Close);
            }

            return progressWindow.ShowDialog();
        }
    }
}
