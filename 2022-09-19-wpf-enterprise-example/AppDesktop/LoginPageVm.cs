using System;

namespace AppDesktop
{
    internal sealed class LoginPageVm : BindableBase
    {
        private string loginId = "";
        public string LoginId
        {
            get => loginId;
            set { loginId = value; RaisePropertyChanged(); LoginCommand.RaiseCanExecuteChanged(); IsFailed = false; }
        }

        private string password = "";
        public string Password
        {
            get => password;
            set { password = value; RaisePropertyChanged(); LoginCommand.RaiseCanExecuteChanged(); IsFailed = false; }
        }

        private bool isFailed;
        public bool IsFailed
        {
            get => isFailed;
            set { isFailed = value; RaisePropertyChanged(); }
        }

        public Command<object?> LoginCommand { get; }

        public event EventHandler<LoginRequest>? OnLoginRequested;

        public void NotifyLoginFailed() => IsFailed = true;

        public LoginPageVm()
        {
            LoginCommand = Command.CreateWithCanExecute<object?>(
                _ => !string.IsNullOrEmpty(LoginId)
                       && !string.IsNullOrEmpty(Password),
                _ => OnLoginRequested?.Invoke(this, new LoginRequest(LoginId, Password))
            );
        }
    }

    internal sealed record LoginRequest(string LoginId, string Password);
}
