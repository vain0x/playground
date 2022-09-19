using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;

namespace AppDesktop
{
    internal sealed class MainWindowVm : BindableBase
    {
        private INotifyPropertyChanged currentPage;
        public INotifyPropertyChanged CurrentPage
        {
            get => currentPage;
            set { currentPage = value; RaisePropertyChagned(); }
        }

        private LoginInfo? loginInfo;
        public LoginInfo? LoginInfo
        {
            get => loginInfo;
            set { loginInfo = value; RaisePropertyChagned(); }
        }

        private readonly List<EmployeeListItem> dummyEmployees =
            "Alice,Bob,Charlotte,Don,Eve"
                .Split(",")
                .Select((name, index) => new EmployeeListItem(1 + index, name))
                .ToList();

        private int lastEmployeeId = 5;

        public MainWindowVm()
        {
            var loginPage = new LoginPageVm();
            loginPage.OnLoginRequested += OnLoginRequested;
            currentPage = loginPage;

#if DEBUG
            // デバッグ用: 毎回ログインするのは面倒なので自動でログインする
            var loginRequest = new LoginRequest("john", "john_password");
            loginPage.LoginId = loginRequest.LoginId;
            loginPage.Password = loginRequest.Password;
            loginPage.LoginCommand.Execute(loginRequest);
#endif
        }

        private void OnLoginRequested(object? _sender, LoginRequest request)
        {
            Debug.WriteLine($"LoginId={request.LoginId} Password={request.Password}");
            LoginInfo = new LoginInfo()
            {
                Username = string.Concat(request.LoginId[..1].ToUpper(), request.LoginId[1..]),
            };
            OpenHomePage();
        }

        private void OpenHomePage()
        {
            var page = new HomePageVm();
            page.GoEmployeesCommand.Executed += (_, _) => OpenEmployeesListPage();

            CurrentPage = page;
        }

        private void OpenEmployeesListPage()
        {
            var page = new EmployeesListPageVm(dummyEmployees.ToArray());
            page.CreateCommand.Executed += (_, _) => OpenEmployeesCreatePage();
            page.OnDeleteRequested += OnDeleteEmployees;
            page.BackCommand.Executed += (_, _) => OpenHomePage();

            CurrentPage = page;
        }

        private void OpenEmployeesCreatePage()
        {
            var page = new EmployeesCreatePageVm();
            page.OnCreateRequested += OnCreateEmployee;
            page.CancelCommand.Executed += (_, _) => OpenEmployeesListPage();
            CurrentPage = page;
        }

        private void OnCreateEmployee(object? _sender, CreateEmployeeRequest request)
        {
            lastEmployeeId++;
            var id = lastEmployeeId;
            var employee = new EmployeeListItem(id, request.EmployeeName);
            dummyEmployees.Add(employee);

            OpenEmployeesListPage();
        }

        private void OnDeleteEmployees(object? _sender, int[] employeeIds)
        {
            dummyEmployees.RemoveAll(e => employeeIds.Contains(e.EmployeeId));

            // TODO: update employeesList in-place
            OpenEmployeesListPage();
        }
    }

    internal sealed class LoginInfo : BindableBase
    {
        private string username = "";
        public string Username
        {
            get => username;
            set { username = value; RaisePropertyChagned(); }
        }
    }
}
