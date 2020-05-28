using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.IO
{
    sealed class RelativeDirectoryPath
        : RelativeFileObjectPath
        , IRelativeDirectoryPath
    {
        public RelativeDirectoryPath(IReadOnlyList<string> path)
            : base(path)
        {
        }

        IFileObjectPath IDirectoryPath.GetChild(string name)
        {
            throw new NotImplementedException();
        }

        IRelativeFileObjectPath IRelativeDirectoryPath.GetChild(string name)
        {
            throw new NotImplementedException();
        }

        IDirectoryPath IDirectoryPath.GetChildDirectory(string name)
        {
            throw new NotImplementedException();
        }

        IRelativeDirectoryPath IRelativeDirectoryPath.GetChildDirectory(string name)
        {
            throw new NotImplementedException();
        }

        IFilePath IDirectoryPath.GetChildFile(string name)
        {
            throw new NotImplementedException();
        }

        IRelativeFilePath IRelativeDirectoryPath.GetChildFile(string name)
        {
            throw new NotImplementedException();
        }

        IAbsoluteDirectoryPath IRelativeDirectoryPath.ToAbsolutePath(IAbsoluteDirectoryPath basePath)
        {
            throw new NotImplementedException();
        }
    }
}
