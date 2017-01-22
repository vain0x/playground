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
        EventHandler<FileSystemChange> changed;

        void OnCreated(object sender, FileSystemEventArgs e)
        {
            if (System.IO.File.Exists(e.FullPath))
            {
                var path = fileSystem.GetVirtualDirectoryPath(e.FullPath);
                 changed?.Invoke(this, FileSystemChange.FromCreated(path));
            }
            else if (System.IO.Directory.Exists(e.FullPath))
            {
                var path = fileSystem.GetVirtualDirectoryPath(e.FullPath);
                changed?.Invoke(this, FileSystemChange.FromCreated(path));
            }
        }

        void OnDeleted(object sender, FileSystemEventArgs e)
        {
            e.
        }

        void Attach()
        {
            if (watcher.EnableRaisingEvents) return;

            watcher.Created += OnCreated;
            watcher.Deleted += OnDeleted;

            watcher.EnableRaisingEvents = true;
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

        event EventHandler<FileSystemChange> IFileSystemWatcher.Changed
        {
            add { changed += value; }
            remove { changed -= value; }
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
