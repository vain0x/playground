using System.Diagnostics;

namespace AppDesktop
{
    internal sealed class TestStub
    {
        public bool Run()
        {
            var main = new MainWindowVm();
            main.LoginInfo = new LoginInfoVm() { Username = "Tester" };

            // TODO: 何をテストするか考える
            Debug.WriteLine("No tests...");
            return true;
        }
    }
}
