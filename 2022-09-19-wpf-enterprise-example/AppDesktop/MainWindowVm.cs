namespace AppDesktop
{
    internal sealed class MainWindowVm : BindableBase
    {
        public LoginPageVm LoginPage { get; } = new();
    }
}
