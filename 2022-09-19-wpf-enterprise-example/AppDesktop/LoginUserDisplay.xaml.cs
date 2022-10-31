using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;

namespace AppDesktop
{
    public partial class LoginUserDisplay : UserControl
    {
        public LoginUserDisplay()
        {
            InitializeComponent();

            contextMenu.SetBinding(DataContextProperty, new Binding(nameof(DataContext)) { Source = this });
        }

        private void OnClick(object _sender, RoutedEventArgs _e)
        {
            contextMenu.IsOpen = true;
        }
    }
}
