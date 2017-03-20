using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SharpFileSystem
{
    /// <summary>
    /// Represents a change of a file/directory.
    /// </summary>
    public struct FileSystemChange
    {
        public System.IO.WatcherChangeTypes ChangeType { get; }

        /// <summary>
        /// Gets the path to the affected file/directory.
        /// It's valid if ChangeType is Deleted, Changed, or Renamed.
        /// </summary>
        public FileSystemPath OldPath { get; }

        /// <summary>
        /// Gets the path to the affected file/directory.
        /// It's valid if ChangeType is Created, Changed, or Renamed.
        /// </summary>
        public FileSystemPath NewPath { get; }

        FileSystemChange(
            System.IO.WatcherChangeTypes changeType,
            FileSystemPath oldPath,
            FileSystemPath newPath
        )
        {
            ChangeType = changeType;
            OldPath = oldPath;
            NewPath = newPath;
        }

        public static FileSystemChange
            FromCreated(FileSystemPath path)
        {
            return new FileSystemChange(System.IO.WatcherChangeTypes.Created, path, path);
        }

        public static FileSystemChange
            FromDeleted(FileSystemPath path)
        {
            return new FileSystemChange(System.IO.WatcherChangeTypes.Deleted, path, path);
        }

        public static FileSystemChange
            FromChanged(FileSystemPath path)
        {
            return new FileSystemChange(System.IO.WatcherChangeTypes.Changed, path, path);
        }

        public static FileSystemChange
            FromRenamed(FileSystemPath oldPath, FileSystemPath newPath)
        {
            var changeType = System.IO.WatcherChangeTypes.Renamed;
            return new FileSystemChange(changeType, oldPath, newPath);
        }
    }
}
