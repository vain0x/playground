using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using Dyxi.CSharp.Util;

namespace Dyxi.Muse.ViewModel
{
    public class MainWindow : ViewModelBase
    {
        public MainWindow()
        {
            _settingsWindow = new SettingsWindow();
        }

        private ICommand _openSettingsCommand;
        public ICommand OpenSettingsCommand
        {
            get { return (_openSettingsCommand ?? (_openSettingsCommand = new LambdaCommand(param => SettingsWindow.Show(NullableUnit.Instance)))); }
        }

        public SettingsWindow SettingsWindow
        {
            get { return _settingsWindow; }
        }

        private SettingsWindow _settingsWindow;
    }
}
