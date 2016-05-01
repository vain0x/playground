using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using NAudio.Wave;
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
            _playingTrack = null;

            _waveOut = new WaveOut();
            _waveOut.PlaybackStopped += PlaybackStopped;
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

        private WaveOut _waveOut;
        private TrackRow _playingTrack;

        private ICommand _playCommand;
        public ICommand PlayCommand
        {
            get { return (_playCommand ?? (_playCommand = new LambdaCommand(param => Play()))); }
        }

        private void Play()
        {
            if (_playingTrack == null)
            {
                if (Stack.IsEmpty()) return;
                _playingTrack = Stack.First();
                var task = MediaCache.FetchAsync(_playingTrack.MediaId);
                task.Wait();
                var reader = new MediaFoundationReader(task.Result);
                _waveOut.Init(reader);
                _waveOut.Play();
            }
            else
            {
                switch (_waveOut.PlaybackState)
                {
                    case PlaybackState.Paused:
                        _waveOut.Resume();
                        break;
                    case PlaybackState.Playing:
                        _waveOut.Pause();
                        break;
                }
            }
        }

        private void PlaybackStopped(object sender, EventArgs e)
        {
            switch (_waveOut.PlaybackState)
            {
                case PlaybackState.Stopped:
                    _playingTrack = null;
                    Stack.Remove(_playingTrack);
                    RaisePropertyChanged("Stack");
                    Play();
                    break;
            }
        }
        
        public SettingsWindow SettingsWindow
        {
            get { return _settingsWindow; }
        }
        
        private SettingsWindow _settingsWindow;
    }
}
