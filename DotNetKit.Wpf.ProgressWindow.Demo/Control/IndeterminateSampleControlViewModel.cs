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
    public class IndeterminateSampleControlViewModel
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
                        throw new Exception("Unexpected TaskStatus.");
                }
            });

            // Invoke an event.
            var h = TaskStarted;
            h.Invoke(this, Tuple.Create(task, cancellationTokenSource));
        }

        public IndeterminateSampleControlViewModel()
        {
            runCommand = Command.Create(Run, () => true);
        }
    }
}
