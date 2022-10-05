using System;

namespace AppDesktop
{
    internal sealed class RecordsCreatePageVm : BindableBase
    {
        private string subject = "新しい日誌";
        public string Subject
        {
            get => subject;
            set { subject = value; RaisePropertyChanged(); }
        }

        private string contents = "";
        public string Contents
        {
            get => contents;
            set { contents = value; RaisePropertyChanged(); }
        }

        private bool isRequested;
        public bool IsRequested
        {
            get => isRequested;
            set { isRequested = value; RaisePropertyChanged(); }
        }

        private bool isBusy;
        public bool IsBusy
        {
            get => isBusy;
            set { isBusy = value; RaisePropertyChanged(); SaveCommand.RaiseCanExecuteChanged(); }
        }

        public EventCommand<object?> CancelCommand { get; }
        public Command<object?> SaveCommand { get; }

        public RecordsAddRequestEffect RequestEffect { get; } = new();

        public RecordsCreatePageVm()
        {
            CancelCommand = EventCommand.Create<object?>(this);

            SaveCommand = Command.CreateWithCanExecute<object?>(
                _ => !IsBusy && Subject != "",
                _ => RequestEffect?.Invoke(new RecordsAddRequest(Subject, Contents))
            );

            RequestEffect.PropertyChanged += (_sender, e) =>
            {
                if (e.PropertyName == nameof(RecordsAddRequestEffect.IsRunning))
                {
                    RaisePropertyChanged(nameof(IsBusy));
                }
            };
        }
    }

    internal record RecordsAddRequest(string Subject, string Contents)
    {
        public Action? OnFinally { get; set; }
    }

    internal sealed class RecordsAddRequestEffect : BindableBase
    {
        private bool isRunning;
        public bool IsRunning
        {
            get => isRunning;
            set { isRunning = value; RaisePropertyChanged(); }
        }

        public event EventHandler<RecordsAddRequest>? Invoked;

        public void Invoke(RecordsAddRequest request)
        {
            request.OnFinally += () =>
            {
                IsRunning = false;
            };

            Invoked?.Invoke(this, request);
        }
    }
}
