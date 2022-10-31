using System.Diagnostics;
using System.Windows;

namespace AppDesktop
{
    public partial class App : Application
    {
        public App()
        {
#if DEBUG
            // デバッグ時のみ: 起動時に自動テストを行う
            Startup += (_, _) =>
            {
                Debug.Assert(new TestStub().Run());
            };
#endif
        }
    }
}
