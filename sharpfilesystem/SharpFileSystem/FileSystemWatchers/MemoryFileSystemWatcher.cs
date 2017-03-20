using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SharpFileSystem.FileSystems;

namespace SharpFileSystem
{
    public sealed class MemoryFileSystemWatcher
        : IFileSystemWatcher
        , IDisposable
    {
        readonly MemoryFileSystem fileSystem;
        readonly FileSystemPath path;

        bool enableRaisingEvents;

        public event EventHandler<FileSystemChange> Changed;

        void RaiseChanged(FileSystemChange change)
        {
            Changed?.Invoke(this, change);
        }

        void OnChanged(object sender, FileSystemChange e)
        {
            switch (e.ChangeType)
            {
                case WatcherChangeTypes.Created:
                    if (e.NewPath.ParentPath == path)
                    {
                        RaiseChanged(e);
                    }
                    break;
                case WatcherChangeTypes.Changed:
                    if (e.NewPath.ParentPath == path)
                    {
                        RaiseChanged(e);
                    }
                    break;
                case WatcherChangeTypes.Deleted:
                    if (e.OldPath.ParentPath == path)
                    {
                        RaiseChanged(e);
                    }
                    break;
                case WatcherChangeTypes.Renamed:
                    if (e.OldPath.ParentPath == path)
                    {
                        if (e.NewPath.ParentPath == path)
                        {
                            RaiseChanged(e);
                        }
                        else
                        {
                            RaiseChanged(FileSystemChange.FromDeleted(e.OldPath));
                        }
                    }
                    else
                    {
                        if (e.NewPath.ParentPath == path)
                        {
                            RaiseChanged(FileSystemChange.FromCreated(e.NewPath));
                        }
                    }
                    break;
            }
        }

        void Attach()
        {
            if (enableRaisingEvents) return;
            enableRaisingEvents = true;

            fileSystem.Changed += OnChanged;
        }

        void Detach()
        {
            if (!enableRaisingEvents) return;
            enableRaisingEvents = false;

            fileSystem.Changed -= OnChanged;
        }

        public void Dispose()
        {
            Detach();
        }

        #region IFileSystemWatcher implementation
        public IFileSystem FileSystem
        {
            get { return fileSystem; }
        }

        public FileSystemPath Path
        {
            get { return path; }
        }

        public bool EnableRaisingEvents
        {
            get { return enableRaisingEvents; }
            set
            {
                if (value)
                {
                    Attach();
                }
                else
                {
                    Detach();
                }
            }
        }
        #endregion

        public MemoryFileSystemWatcher(MemoryFileSystem fileSystem, FileSystemPath path)
        {
            this.fileSystem = fileSystem;
            this.path = path;
        }
    }
}
