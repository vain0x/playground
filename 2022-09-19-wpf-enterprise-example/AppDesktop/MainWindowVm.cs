using System;
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
            set { currentPage = value; RaisePropertyChanged(); }
        }

        private LoginInfo? loginInfo;
        public LoginInfo? LoginInfo
        {
            get => loginInfo;
            set { loginInfo = value; RaisePropertyChanged(); }
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
            loginPage.OnLoginRequested += (_, request) => Login(request, loginPage);
            currentPage = loginPage;

#if DEBUG
            // デバッグ用: 毎回ログインするのは面倒なので自動でログインする
            var loginRequest = new LoginRequest("john", "john_password");
            loginPage.LoginId = loginRequest.LoginId;
            loginPage.Password = loginRequest.Password;
            loginPage.LoginCommand.Execute(loginRequest);
#endif
        }

        private void Login(LoginRequest request, LoginPageVm page)
        {
            Debug.WriteLine($"LoginId={request.LoginId} Password={request.Password}");
            LoginInfo = new LoginInfo()
            {
                Username = string.Concat(request.LoginId[..1].ToUpper(), request.LoginId[1..]),
            };
            if (request.Password == "password")
            {
                page.OnLoginFailed();
                return;
            }
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
            page.OnDeleteRequested += (_, ids) => DeleteEmployees(ids);
            page.BackCommand.Executed += (_, _) => OpenHomePage();

            CurrentPage = page;
        }

        private void OpenEmployeesCreatePage()
        {
            var page = new EmployeesCreatePageVm();
            page.OnCreateRequested += (_, request) => CreateEmployee(request);
            page.CancelCommand.Executed += (_, _) => OpenEmployeesListPage();
            CurrentPage = page;
        }

        private void CreateEmployee(CreateEmployeeRequest request)
        {
            lastEmployeeId++;
            var id = lastEmployeeId;
            var employee = new EmployeeListItem(id, request.EmployeeName);
            dummyEmployees.Add(employee);

            OpenEmployeesListPage();
        }

        private void DeleteEmployees(int[] employeeIds)
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
            set { username = value; RaisePropertyChanged(); }
        }
    }
}
