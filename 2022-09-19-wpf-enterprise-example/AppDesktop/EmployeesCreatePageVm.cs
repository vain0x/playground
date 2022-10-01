using System;

namespace AppDesktop
{
    internal sealed class EmployeesCreatePageVm : BindableBase
    {
        private string employeeName = "";
        public string EmployeeName
        {
            get => employeeName;
            set { employeeName = value; RaisePropertyChanged(); CreateCommand.RaiseCanExecuteChanged(); }
        }

        public Command<object?> CreateCommand { get; }

        public event EventHandler<CreateEmployeeRequest>? OnCreateRequested;

        public EventCommand<object?> CancelCommand { get; }

        public EmployeesCreatePageVm()
        {
            CreateCommand = Command.CreateWithCanExecute<object?>(
                _ => !string.IsNullOrEmpty(EmployeeName),
                _ => OnCreateRequested?.Invoke(this, new CreateEmployeeRequest(EmployeeName))
            );

            CancelCommand = EventCommand.Create<object?>(this);
        }
    }

    internal sealed record CreateEmployeeRequest(string EmployeeName);
}
