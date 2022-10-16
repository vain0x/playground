using System;

namespace AppDesktop
{
    internal sealed class UsersPasswordChangePageVm : BindableBase
    {
        private string currentPassword = "";
        public string CurrentPassword
        {
            get => currentPassword;
            set { currentPassword = value; RaisePropertyChanged(); SaveCommand.RaiseCanExecuteChanged(); }
        }


        private string newPassword = "";
        public string NewPassword
        {
            get => newPassword;
            set { newPassword = value; RaisePropertyChanged(); SaveCommand.RaiseCanExecuteChanged(); }
        }

        public Command<object?> SaveCommand { get; }
        public event EventHandler<PasswordChangeRequest>? Requested;

        public UsersPasswordChangePageVm()
        {
            SaveCommand = Command.CreateWithCanExecute<object?>(
                _ => !string.IsNullOrEmpty(CurrentPassword)
                    && !string.IsNullOrEmpty(NewPassword),
                _ => Requested?.Invoke(this, new PasswordChangeRequest(CurrentPassword, NewPassword))
            );
        }
    }
}
