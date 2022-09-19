using System;

namespace AppDesktop
{
    internal sealed class LoginPageVm : BindableBase
    {
        private string loginId = "";
        public string LoginId
        {
            get => loginId;
            set { loginId = value; RaisePropertyChagned(); LoginCommand.RaiseCanExecuteChanged(); }
        }

        private string password = "";
        public string Password
        {
            get => password;
            set { password = value; RaisePropertyChagned(); LoginCommand.RaiseCanExecuteChanged(); }
        }

        public Command<object> LoginCommand { get; }

        public LoginPageVm()
        {
            LoginCommand = Command.CreateWithCanExecute<object>(
                _ =>
                {
                    System.Diagnostics.Debug.WriteLine($"canExecute? {loginId}, {Password}");
                    return !string.IsNullOrEmpty(LoginId)
                       && !string.IsNullOrEmpty(Password);
                },
                _ =>
                {
                    System.Diagnostics.Debug.WriteLine($"login! '{LoginId}' '{Password}'");
                });
        }
    }
}
