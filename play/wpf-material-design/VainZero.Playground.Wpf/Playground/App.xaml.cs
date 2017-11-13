using System;
using System.Collections.Generic;
using System.Configuration;
using System.Data;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;

namespace VainZero.Playground
{
    /// <summary>
    /// App.xaml の相互作用ロジック
    /// </summary>
    public partial class App : Application
    {
        protected override void OnStartup(StartupEventArgs eventArgs)
        {
            DispatcherUnhandledException += (sender, e) =>
            {
                Debug.WriteLine(e.Exception);
            };

            base.OnStartup(eventArgs);
        }
    }
}
