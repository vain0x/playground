using System.Windows;
using System.Windows.Controls;

namespace AppDesktop
{
    public partial class LoginPage : UserControl
    {
        public LoginPage()
        {
            InitializeComponent();
        }

        private void OnPasswordChanged(object sender, RoutedEventArgs e)
        {
            var passwordBox = (PasswordBox)sender;
            var vm = (LoginPageVm)DataContext;

            vm.Password = passwordBox.Password;
        }
    }
}
