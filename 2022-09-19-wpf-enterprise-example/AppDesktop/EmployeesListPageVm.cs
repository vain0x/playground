using System.Collections.ObjectModel;
using System.Linq;

namespace AppDesktop
{
    internal sealed class EmployeesListPageVm : BindableBase
    {
        public ObservableCollection<EmployeeListItemVm> Employees { get; }

        public EventCommand<object?> BackCommand { get; }

        public EmployeesListPageVm(EmployeeListItem[] employees)
        {
            Employees = new(employees.Select(e => new EmployeeListItemVm(e.EmployeeId, e.EmployeeName)).ToArray());
            BackCommand = EventCommand.Create<object?>(this);
        }
    }

    // FIXME: モデル層に置く
    internal sealed record EmployeeListItem(int EmployeeId, string EmployeeName);

    internal sealed class EmployeeListItemVm : BindableBase
    {
        public int EmployeeId { get; }

        private string employeeName;
        public string EmployeeName
        {
            get => employeeName;
            set { employeeName = value; RaisePropertyChagned(); }
        }

        public EmployeeListItemVm(int employeeId, string employeeName)
        {
            EmployeeId = employeeId;
            this.employeeName = employeeName;
        }
    }
}
