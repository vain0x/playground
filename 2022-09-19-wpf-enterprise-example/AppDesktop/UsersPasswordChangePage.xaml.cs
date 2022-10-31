using System.Windows.Controls;

namespace AppDesktop
{
    public partial class UsersPasswordChangePage : UserControl
    {
        private UsersPasswordChangePageVm Vm => (UsersPasswordChangePageVm)DataContext;

        public UsersPasswordChangePage()
        {
            InitializeComponent();

            currentPasswordBox.PasswordChanged += (_, _) =>
            {
                Vm.CurrentPassword = currentPasswordBox.Password;
            };

            newPasswordBox.PasswordChanged += (_, _) =>
            {
                Vm.NewPassword = newPasswordBox.Password;
            };
        }
    }
}
