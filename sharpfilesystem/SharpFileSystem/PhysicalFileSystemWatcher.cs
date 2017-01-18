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

        #region IFileSystemWatcher implementation
        public IFileSystem FileSystem
        {
            get { return fileSystem; }
        }

        public bool EnableRaisingEvents
        {
            get { return watcher.EnableRaisingEvents; }
            set { watcher.EnableRaisingEvents = value; }
        }

        FileSystemPath IFileSystemWatcher.Path
        {
            get { return path; }
        }

        public event FileSystemEventHandler Changed
        {
            add { watcher.Changed += value; }
            remove { watcher.Changed -= value; }
        }

        public event FileSystemEventHandler Created
        {
            add { watcher.Created += value; }
            remove { watcher.Created -= value; }
        }

        public event FileSystemEventHandler Deleted
        {
            add { watcher.Deleted += value; }
            remove { watcher.Deleted -= value; }
        }

        public event RenamedEventHandler Renamed
        {
            add { watcher.Renamed += value; }
            remove { watcher.Renamed -= value; }
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
