using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Input;
using DotNetKit.Functional;

namespace DotNetKit.Wpf.Demo
{
    public class MinimumSampleControlViewModel
        : INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        string message;
        public string Message
        {
            get { return message; }
            set
            {
                message = value;

                var h = PropertyChanged;
                if (h != null) h.Invoke(this, new PropertyChangedEventArgs("Message"));
            }
        }

        readonly ICommand runCommand;
        public ICommand RunCommand
        {
            get { return runCommand; }
        }

        public event EventHandler<Tuple<Task, CancellationTokenSource>> TaskStarted;

        async Task RunAsync(CancellationToken cancellationToken)
        {
            // Suspend 5 seconds.
            await Task.Delay(5000, cancellationToken);
        }

        void Run()
        {
            // Create an object to cancel the task.
            var cancellationTokenSource = new CancellationTokenSource();

            // Create and start a new task which just suspends 5 seconds.
            var task = RunAsync(cancellationTokenSource.Token);

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
                        Message = "Unknown TaskStatus.";
                        break;
                }
            });

            // Invoke an event.
            var h = TaskStarted;
            h.Invoke(this, Tuple.Create(task, cancellationTokenSource));
        }

        public MinimumSampleControlViewModel()
        {
            runCommand = Command.Create(Run, () => true);
        }
    }
}
