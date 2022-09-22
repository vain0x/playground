using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Windows;

namespace AppDesktop
{
    internal sealed class EmployeesListPageVm : BindableBase
    {
        public ObservableCollection<EmployeeListItemVm> Employees { get; }

        public EventCommand<object?> CreateCommand { get; }
        public Command<object?> DeleteCommand { get; }
        public EventCommand<object?> BackCommand { get; }

        public event EventHandler<int[]>? OnDeleteRequested;

        public EmployeesListPageVm(EmployeeListItem[] employees)
        {
            Employees = new(employees.Select(e => new EmployeeListItemVm(e.EmployeeId, e.EmployeeName)).ToArray());

            CreateCommand = EventCommand.Create<object?>(this);

            DeleteCommand = Command.CreateWithCanExecute<object?>(
                _ => SomeRowIsChecked(),
                _ =>
                {
                    var items = CheckedItems().ToArray();
                    var ids = items.Select(e => e.EmployeeId).ToArray();
                    var names = string.Join(", ", items.Select(e => e.EmployeeName));

                    var confirmed = MessageBox.Show($"以下の社員を削除します。よろしいですか？\n{names}", "", MessageBoxButton.YesNo) == MessageBoxResult.Yes;
                    if (confirmed)
                    {
                        OnDeleteRequested?.Invoke(this, ids);
                    }
                }
            );

            BackCommand = EventCommand.Create<object?>(this);

            // DeleteCommand depends on all of Employees[].Checked.
            foreach (var em in Employees)
            {
                em.PropertyChanged += (_, e) =>
                {
                    if (e.PropertyName == nameof(EmployeeListItemVm.Checked))
                    {
                        DeleteCommand.RaiseCanExecuteChanged();
                    }
                };
            }
        }

        private bool SomeRowIsChecked() => Employees.Any(e => e.Checked);

        private IEnumerable<EmployeeListItemVm> CheckedItems() => Employees.Where(e => e.Checked);
    }

    // FIXME: モデル層に置く
    internal sealed record EmployeeListItem(int EmployeeId, string EmployeeName);

    internal sealed class EmployeeListItemVm : BindableBase
    {
        public int EmployeeId { get; }

        private bool @checked;
        public bool Checked
        {
            get => @checked;
            set { @checked = value; RaisePropertyChanged(); }
        }

        private string employeeName;
        public string EmployeeName
        {
            get => employeeName;
            set { employeeName = value; RaisePropertyChanged(); }
        }

        public EmployeeListItemVm(int employeeId, string employeeName)
        {
            EmployeeId = employeeId;
            this.employeeName = employeeName;
        }
    }
}
