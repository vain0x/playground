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
            _stack = new List<TrackRow>();
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

                Tracks = _coll.Items.Select(media =>
                    new TrackRow()
                    {
                        Title = media.name,
                        Extension = media.extension,
                        MediaId = media.id
                    }
                ).ToArray();
            }
        }
        
        private TrackRow[] _tracks;
        public TrackRow[] Tracks
        {
            get { return _tracks; }
            set
            {
                _tracks = value;
                RaisePropertyChanged();
            }
        }

        private List<TrackRow> _stack;
        public List<TrackRow> Stack
        {
            get { return _stack; }
            set
            {
                _stack = value;
                RaisePropertyChanged();
            }
        }

        public void PushToStack(TrackRow row)
        {
            Stack.Insert(0, row);
            MediaCache.FetchAsync(row.MediaId);
            RaisePropertyChanged("Stack");
        }
        
        public SettingsWindow SettingsWindow
        {
            get { return _settingsWindow; }
        }
        
        private SettingsWindow _settingsWindow;
    }
}
