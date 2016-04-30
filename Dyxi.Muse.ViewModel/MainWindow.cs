using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using Dyxi.CSharp.Util;
using Dyxi.Muse.Model;

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

        private IColl _coll;
        public IColl Coll
        {
            get { return _coll; }
            set
            {
                _coll = value;
                RaisePropertyChanged();
                Tracks = _coll.Items.ToArray();
            }
        }
        
        private media[] _tracks;
        public media[] Tracks
        {
            get { return _tracks; }
            set
            {
                _tracks = value;
                RaisePropertyChanged();
            }
        }

        public SettingsWindow SettingsWindow
        {
            get { return _settingsWindow; }
        }
        
        private SettingsWindow _settingsWindow;
    }
}
