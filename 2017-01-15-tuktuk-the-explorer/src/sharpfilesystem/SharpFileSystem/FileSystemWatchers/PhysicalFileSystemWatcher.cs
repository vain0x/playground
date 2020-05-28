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

        readonly HashSet<string> directoryNameSet =
            new HashSet<string>();

        void RefreshDirectoryNameSet()
        {
            directoryNameSet.Clear();

            foreach (var entity in fileSystem.GetEntities(path))
            {
                if (entity.IsFile) break;
                directoryNameSet.Add(entity.EntityName);
            }
        }

        FileSystemPath GetVirtualPath(string fullPath)
        {
            var name = Path.GetFileName(fullPath);
            return
                directoryNameSet.Contains(name)
                    ? fileSystem.GetVirtualDirectoryPath(fullPath)
                    : fileSystem.GetVirtualFilePath(fullPath);
        }

        void OnCreated(object sender, FileSystemEventArgs e)
        {
            if (System.IO.Directory.Exists(e.FullPath))
            {
                directoryNameSet.Add(Path.GetFileName(e.FullPath));
            }

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
            var path = GetVirtualPath(e.FullPath);

            if (path.IsDirectory)
            {
                directoryNameSet.Remove(path.EntityName);
            }

            RaiseChanged(FileSystemChange.FromDeleted(path));
        }

        void OnRenamed(object sender, RenamedEventArgs e)
        {
            var oldPath = GetVirtualPath(e.OldFullPath);
            var newPath = GetVirtualPath(e.FullPath);

            if (oldPath.IsDirectory)
            {
                directoryNameSet.Remove(oldPath.EntityName);
            }
            if (newPath.IsDirectory)
            {
                directoryNameSet.Add(newPath.EntityName);
            }

            RaiseChanged(FileSystemChange.FromRenamed(oldPath, newPath));
        }

        void Attach()
        {
            if (watcher.EnableRaisingEvents) return;
            watcher.EnableRaisingEvents = true;

            RefreshDirectoryNameSet();

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
