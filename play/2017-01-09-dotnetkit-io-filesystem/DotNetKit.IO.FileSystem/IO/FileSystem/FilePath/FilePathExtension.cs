using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.IO.FileSystem
{
    public static class FilePathExtension
    {
        public static bool IsAbsolutePath(this IFileObjectPath @this)
        {
            return @this is IAbsoluteFileObjectPath;
        }

        public static bool IsRelativePath(this IFileObjectPath @this)
        {
            return @this is IRelativeFileObjectPath;
        }
    }
}
