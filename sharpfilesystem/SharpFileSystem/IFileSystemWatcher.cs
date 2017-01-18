using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SharpFileSystem.FileSystems;

namespace SharpFileSystem
{
    public interface IFileSystemWatcher
        : IDisposable
    {
        IFileSystem FileSystem { get; }
        FileSystemPath Path { get; }
        bool EnableRaisingEvents { get; set; }

        event FileSystemEventHandler Changed;
        event FileSystemEventHandler Created;
        event FileSystemEventHandler Deleted;
        event RenamedEventHandler Renamed;
    }
}
