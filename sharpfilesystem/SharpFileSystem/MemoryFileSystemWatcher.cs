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
        readonly Action dispose;

        #region IFileSystemWatcher implementation
        public IFileSystem FileSystem
        {
            get { return fileSystem; }
        }

        public FileSystemPath Path
        {
            get { return path; }
        }

        public bool EnableRaisingEvents { get; set; }

        public event FileSystemEventHandler Changed;
        public event FileSystemEventHandler Created;
        public event FileSystemEventHandler Deleted;
        public event RenamedEventHandler Renamed;
        #endregion

        public void Dispose()
        {
            dispose();
        }

        FileSystemEventHandler FileSystemEventHandler(Action<FileSystemEventArgs> raise)
        {
            return
                (sender, e) =>
                {
                    var path = FileSystemPath.Parse(e.FullPath);
                    if (path.ParentPath == this.path)
                    {
                        raise(e);
                    }
                };
        }

        void OnRenamed(object sender, RenamedEventArgs e)
        {
            var oldPath = FileSystemPath.Parse(e.OldFullPath);
            var newPath = FileSystemPath.Parse(e.FullPath);
            if (oldPath.ParentPath == path)
            {
                if (newPath.ParentPath == path)
                {
                    Renamed?.Invoke(this, e);
                }
                else
                {
                    Deleted?.Invoke(this, new FileSystemEventArgs(WatcherChangeTypes.Deleted, path.ToString(), oldPath.ToString()));
                }
            }
            else
            {
                if (newPath.ParentPath == path)
                {
                    Created?.Invoke(this, new FileSystemEventArgs(WatcherChangeTypes.Created, path.ToString(), newPath.ToString()));
                }
            }
        }

        public MemoryFileSystemWatcher(MemoryFileSystem fileSystem, FileSystemPath path)
        {
            this.fileSystem = fileSystem;
            this.path = path;

            var onChanged = FileSystemEventHandler(e => Changed?.Invoke(this, e));
            var onCreated = FileSystemEventHandler(e => Created?.Invoke(this, e));
            var onDeleted = FileSystemEventHandler(e => Deleted?.Invoke(this, e));
            fileSystem.Changed += onChanged;
            fileSystem.Created += onCreated;
            fileSystem.Deleted += onDeleted;
            fileSystem.Renamed += OnRenamed;

            dispose =
                new Action(() =>
                {
                    fileSystem.Changed -= onChanged;
                    fileSystem.Created -= onCreated;
                    fileSystem.Deleted -= onDeleted;
                    fileSystem.Renamed -= OnRenamed;
                });
        }
    }
}
