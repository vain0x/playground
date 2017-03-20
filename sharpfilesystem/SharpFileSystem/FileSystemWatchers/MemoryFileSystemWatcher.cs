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

        public event EventHandler<FileSystemChange> Changed;
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

        void RaiseChanged(FileSystemChange change)
        {
            Changed?.Invoke(this, change);
        }

        FileSystemEventHandler
            FileSystemEventHandler(Func<FileSystemPath, FileSystemChange> changeFromPath)
        {
            return
                (sender, e) =>
                {
                    var path = ParsePath(e.FullPath);
                    if (path.ParentPath == this.path)
                    {
                        RaiseChanged(changeFromPath(path));
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
                    RaiseChanged(FileSystemChange.FromRenamed(oldPath, newPath));
                }
                else
                {
                    RaiseChanged(FileSystemChange.FromDeleted(oldPath));
                }
            }
            else
            {
                if (newPath.ParentPath == path)
                {
                    RaiseChanged(FileSystemChange.FromCreated(newPath));
                }
            }
        }

        void Attach()
        {
            if (enableRaisingEvents) return;
            enableRaisingEvents = true;

            var onChanged = FileSystemEventHandler(FileSystemChange.FromChanged);
            var onCreated = FileSystemEventHandler(FileSystemChange.FromCreated);
            var onDeleted = FileSystemEventHandler(FileSystemChange.FromDeleted);
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
