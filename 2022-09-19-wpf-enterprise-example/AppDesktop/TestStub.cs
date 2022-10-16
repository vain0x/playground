using System.Diagnostics;

namespace AppDesktop
{
    internal sealed class TestStub
    {
        public bool Run()
        {
            var main = new MainWindowVm();
            main.LoginInfo = new LoginInfo() { Username = "Tester" };

            // TODO: 何をテストするか考える
            Debug.WriteLine("Test.");
            return true;
        }
    }
}
