using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.IO
{
    sealed class RelativeFileObjectPath
        : IRelativeFileObjectPath
    {
        readonly ImmutableArray<string> path;

        public IRelativeDirectoryPath Parent
        {
            get
            {
                if (path.Length == 0 || path.Last() == "..")
                {
                    return new RelativeDirectoryPath(path.Add(".."));
                }
                else
                {
                    return new RelativeDirectoryPath(path.RemoveAt(path.Length - 1));
                }
            }
        }

        IDirectoryPath IFileObjectPath.Parent
        {
            get { return Parent; }
        }

        IRelativeDirectoryPath IRelativeFileObjectPath.Parent
        {
            get { return Parent; }
        }

        IAbsoluteFileObjectPath IRelativeFileObjectPath.ToAbsolutePath(IAbsoluteDirectoryPath basePath)
        {
            throw new NotImplementedException();
        }

        public override string ToString()
        {
            var array = path as string[] ?? path.ToArray();
            return Path.Combine(array);
        }

        static bool IsRegularized(IReadOnlyList<string> path)
        {
            var containsName = false;

            for (var i = 0; i < path.Count; i++)
            {
                if (string.IsNullOrEmpty(path[i]) || path[i] == ".")
                {
                    return true;
                }
                else if (path[i] == "..")
                {
                    return containsName;
                }
                else
                {
                    containsName = true;
                }
            }

            return false;
        }

        static IReadOnlyList<string> Regularize(IReadOnlyList<string> path)
        {
            if (IsRegularized(path))
            {
                return path;
            }

            var result = new List<string>();
            for (var i = 0; i < path.Count; i++)
            {
                var name = path[i];
                if (name == "..")
                {
                    if (result.Count == 0 || result[result.Count - 1] == "..")
                    {
                        result.Add(name);
                    }
                    else
                    {
                        result.RemoveAt(result.Count - 1);
                    }
                }
                else if (!string.IsNullOrEmpty(name) && name != ".")
                {
                    result.Add(name);
                }
            }

            return result;
        }

        public RelativeFileObjectPath(IReadOnlyList<string> path)
        {
            this.path = Regularize(path);
        }
    }
}
