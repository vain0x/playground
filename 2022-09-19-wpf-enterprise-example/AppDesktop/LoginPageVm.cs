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

        public event EventHandler<LoginRequest>? OnLoginRequested;

        public LoginPageVm()
        {
            LoginCommand = Command.CreateWithCanExecute<object>(
                _ => !string.IsNullOrEmpty(LoginId)
                       && !string.IsNullOrEmpty(Password),
                _ => OnLoginRequested?.Invoke(this, new LoginRequest(LoginId, Password))
            );
        }
    }

    internal sealed record LoginRequest(string LoginId, string Password);
}
