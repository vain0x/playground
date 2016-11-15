using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Input;
using DotNetKit.Functional;

namespace DotNetKit.Wpf.Demo
{
    public class ProgressRateSampleControlViewModel
        : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            var h = PropertyChanged;
            if (h != null) h.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        string message;
        public string Message
        {
            get { return message; }
            set
            {
                message = value;
                OnPropertyChanged();
            }
        }

        readonly ICommand runCommand;
        public ICommand RunCommand
        {
            get { return runCommand; }
        }

        public event EventHandler<Tuple<Task, CancellationTokenSource, Progress<double>>> TaskStarted;

        async Task RunAsync(CancellationToken cancellationToken, IProgress<double> progress)
        {
            // Suspend 5 seconds, reporting progress.
            var seconds = 5;
            for (var i = 0; i < seconds; i++)
            {
                // Stop execution if cancellation is requested.
                if (cancellationToken.IsCancellationRequested) return;

                // Report the current progress as a percentage.
                var rate = 100.0 * i / seconds;
                progress.Report(rate);

                // Suspend a second.
                await Task.Delay(1000, cancellationToken);
            }

            // Report the progress has completed.
            progress.Report(100.0);
        }

        void Run()
        {
            var progress = new Progress<double>();

            // Create an object to cancel the task.
            var cancellationTokenSource = new CancellationTokenSource();

            // Create and start a new task which just suspends 5 seconds.
            var task = RunAsync(cancellationTokenSource.Token, progress);

            Message = "Running...";

            // When the task is completed, update the message.
            task.ContinueWith(_ =>
            {
                switch (task.Status)
                {
                    case TaskStatus.RanToCompletion:
                        Message = "Completed.";
                        break;
                    case TaskStatus.Canceled:
                        Message = "Canceled.";
                        break;
                    default:
                        throw new Exception("Unexpected TaskStatus.");
                }
            });

            // Invoke an event.
            var h = TaskStarted;
            h.Invoke(this, Tuple.Create(task, cancellationTokenSource, progress));
        }

        public ProgressRateSampleControlViewModel()
        {
            runCommand = Command.Create(Run, () => true);
        }
    }
}
