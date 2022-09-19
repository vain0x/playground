using System.ComponentModel;
using System.Diagnostics;

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

        public MainWindowVm()
        {
            var loginPage = new LoginPageVm();

            currentPage = loginPage;

            loginPage.OnLoginRequested += (_sender, request) =>
            {
                Debug.WriteLine($"LoginId={request.LoginId} Password={request.Password}");
                CurrentPage = new HomePageVm();
            };
        }
    }
}
