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

        public LoginPageVm()
        {
            LoginCommand = Command.CreateWithCanExecute<object?>(
                _ => !string.IsNullOrEmpty(LoginId)
                       && !string.IsNullOrEmpty(Password),
                _ => OnLoginRequested?.Invoke(this, new LoginRequest(LoginId, Password, OnLoginFailed))
            );
        }

        public void OnLoginFailed()
        {
            IsFailed = true;
        }
    }

    internal sealed class LoginRequest
    {
        public string LoginId { get; }
        public string Password { get; }
        public Action OnFailed { get; }

        public LoginRequest(string loginId, string password, Action onFailed)
        {
            LoginId = loginId;
            Password = password;
            OnFailed = onFailed;
        }
    }
}
