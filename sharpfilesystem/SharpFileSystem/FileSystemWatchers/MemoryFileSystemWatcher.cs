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

        Action detach;

        #region IFileSystemWatcher implementation
        public IFileSystem FileSystem
        {
            get { return fileSystem; }
        }

        public FileSystemPath Path
        {
            get { return path; }
        }

        bool enableRaisingEvents;
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

        public event FileSystemEventHandler Changed;
        public event FileSystemEventHandler Created;
        public event FileSystemEventHandler Deleted;
        public event RenamedEventHandler Renamed;
        #endregion

        public void Dispose()
        {
            Detach();
        }

        FileSystemPath ParsePath(string path)
        {
            var separator = FileSystemPath.DirectorySeparator;
            var fullPath =
                path
                .Replace(System.IO.Path.DirectorySeparatorChar, separator)
                .Replace(System.IO.Path.AltDirectorySeparatorChar, separator)
                .Replace(string.Concat(separator, separator), separator.ToString());
            return FileSystemPath.Parse(fullPath);
        }

        FileSystemEventHandler FileSystemEventHandler(Action<FileSystemEventArgs> raise)
        {
            return
                (sender, e) =>
                {
                    var path = ParsePath(e.FullPath);
                    if (path.ParentPath == this.path)
                    {
                        raise(e);
                    }
                };
        }

        void OnRenamed(object sender, RenamedEventArgs e)
        {
            var oldPath = ParsePath(e.OldFullPath);
            var newPath = ParsePath(e.FullPath);
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

        void Attach()
        {
            if (enableRaisingEvents) return;
            enableRaisingEvents = true;

            var onChanged = FileSystemEventHandler(e => Changed?.Invoke(this, e));
            var onCreated = FileSystemEventHandler(e => Created?.Invoke(this, e));
            var onDeleted = FileSystemEventHandler(e => Deleted?.Invoke(this, e));
            fileSystem.Changed += onChanged;
            fileSystem.Created += onCreated;
            fileSystem.Deleted += onDeleted;
            fileSystem.Renamed += OnRenamed;

            detach =
                new Action(() =>
                {
                    fileSystem.Changed -= onChanged;
                    fileSystem.Created -= onCreated;
                    fileSystem.Deleted -= onDeleted;
                    fileSystem.Renamed -= OnRenamed;
                });
        }

        void Detach()
        {
            if (!enableRaisingEvents) return;
            enableRaisingEvents = false;
            detach();
        }

        public MemoryFileSystemWatcher(MemoryFileSystem fileSystem, FileSystemPath path)
        {
            this.fileSystem = fileSystem;
            this.path = path;
        }
    }
}
