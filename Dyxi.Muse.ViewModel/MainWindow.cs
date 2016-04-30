using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Dyxi.CSharp.Util;

namespace Dyxi.Muse.ViewModel
{
    public class MainWindow : ViewModelBase
    {
        public MainWindow()
        {
            _settingsWindow = new SettingsWindow();
        }

        public SettingsWindow SettingsWindow
        {
            get { return _settingsWindow; }
        }

        private SettingsWindow _settingsWindow;
    }
}
