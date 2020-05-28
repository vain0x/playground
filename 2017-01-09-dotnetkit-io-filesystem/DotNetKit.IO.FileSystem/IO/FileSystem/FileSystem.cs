using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.IO.FileSystem
{
    public abstract class FileSystem
    {
        public abstract IReadOnlyList<string> DirectorySeparators { get; }
        public abstract IReadOnlyCollection<char> InvalidChars { get; }

        public abstract bool IsFile(IFileObjectPath path);
        public abstract bool Is
    }
}
