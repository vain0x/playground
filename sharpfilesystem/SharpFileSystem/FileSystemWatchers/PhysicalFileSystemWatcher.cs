using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SharpFileSystem.FileSystems;

namespace SharpFileSystem
{
    public sealed class PhysicalFileSystemWatcher
        : IFileSystemWatcher
    {
        readonly PhysicalFileSystem fileSystem;
        readonly FileSystemPath path;
        readonly FileSystemWatcher watcher;

        public event EventHandler<FileSystemChange> Changed;

        void RaiseChanged(FileSystemChange change)
        {
            Changed?.Invoke(this, change);
        }

        FileSystemPath GetVirtualPath(string fullPath)
        {
            if (System.IO.File.Exists(fullPath))
            {
                return fileSystem.GetVirtualFilePath(fullPath);
            }
            else if (System.IO.Directory.Exists(fullPath))
            {
                return fileSystem.GetVirtualDirectoryPath(fullPath);
            }
            else
            {
                // TODO: We need to use a dictionary or something to discriminate.
                throw new NotSupportedException();
            }
        }

        void OnCreated(object sender, FileSystemEventArgs e)
        {
            var path = GetVirtualPath(e.FullPath);
            RaiseChanged(FileSystemChange.FromCreated(path));
        }

        void OnChanged(object sender, FileSystemEventArgs e)
        {
            var path = GetVirtualPath(e.FullPath);
            RaiseChanged(FileSystemChange.FromChanged(path));
        }

        void OnDeleted(object sender, FileSystemEventArgs e)
        {
            // TODO: Invoke changed.
        }

        void OnRenamed(object sender, RenamedEventArgs e)
        {
            // TODO: Invoke changed.
        }

        void Attach()
        {
            if (watcher.EnableRaisingEvents) return;
            watcher.EnableRaisingEvents = true;

            watcher.Created += OnCreated;
            watcher.Changed += OnChanged;
            watcher.Deleted += OnDeleted;
            watcher.Renamed += OnRenamed;
        }

        void Detach()
        {
            if (!watcher.EnableRaisingEvents) return;
            watcher.EnableRaisingEvents = false;

            watcher.Created -= OnCreated;
            watcher.Changed -= OnChanged;
            watcher.Deleted -= OnDeleted;
            watcher.Renamed -= OnRenamed;
        }

        #region IFileSystemWatcher implementation
        public IFileSystem FileSystem
        {
            get { return fileSystem; }
        }

        public bool EnableRaisingEvents
        {
            get { return watcher.EnableRaisingEvents; }
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

        FileSystemPath IFileSystemWatcher.Path
        {
            get { return path; }
        }

        public void Dispose()
        {
            watcher.Dispose();
        }
        #endregion

        public PhysicalFileSystemWatcher(PhysicalFileSystem fileSystem, FileSystemPath path)
        {
            this.fileSystem = fileSystem;
            this.path = path;
            watcher = new FileSystemWatcher(fileSystem.GetPhysicalPath(path));
        }
    }
}
