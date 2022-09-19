using System.ComponentModel;
using System.Diagnostics;
using System;

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

            page.GoEmployeesCommand.Executed += (_, _) =>
            {
                Debug.WriteLine("TODO: Navigate to the employees page");
            };

            currentPage = page;
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
