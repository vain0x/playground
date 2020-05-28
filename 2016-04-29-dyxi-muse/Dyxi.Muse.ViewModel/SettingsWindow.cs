using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using Dyxi.CSharp.Util;

namespace Dyxi.Muse.ViewModel
{
    public class SettingsWindow
        : DialogViewModelBase<NullableUnit>
    {
        public string Path { get; set; }

        private string _error = "";
        public string Error
        {
            get { return _error; }
            set
            {
                _error = value;
                RaisePropertyChanged();
            }
        }

        private ICommand _uploadMusicFileCommand;
        public ICommand UploadMusicFileCommand
        {
            get { return (_uploadMusicFileCommand ?? (_uploadMusicFileCommand = new LambdaCommand(param => UploadMusicFile()))); }
        }

        private void UploadMusicFile()
        {
            try
            {
                Model.Import.ImportMediaFile(Path);
            }
            catch (Exception e)
            {
                Error = e.Message;
            }
        }
    }
}
