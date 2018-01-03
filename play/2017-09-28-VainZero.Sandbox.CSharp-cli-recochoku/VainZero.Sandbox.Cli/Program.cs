using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Mail;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using System.Web;

namespace VainZero.Sandbox
{
    public sealed class Program
    {
        IEnumerable<FileInfo> FileIterator(string name)
        {
            if (File.Exists(name))
            {
                yield return new FileInfo(name);
            }
            else if (Directory.Exists(name))
            {
                foreach (var subname in Directory.GetFiles(name))
                {
                    foreach (var file in FileIterator(subname))
                    {
                        yield return file;
                    }
                }
            }
            else
            {
                Console.WriteLine("WARNING: Unknown parameter '" + name + "'.");
            }
        }

        static string Sanitize(string fileName)
        {
            var sb = new StringBuilder(fileName);

            foreach (var ch in Path.GetInvalidFileNameChars().Concat(Path.GetInvalidPathChars()))
            {
                sb.Replace(ch, '_');
            }

            return sb.ToString();
        }

        sealed class RenameTarget
        {
            public FileInfo File { get; }

            public long TrackId { get; }
            string TrackIdAsString => TrackId.ToString("D2");

            public string Album { get; }
            public string Title { get; }

            public string NewFileName()
            {
                return $"{TrackIdAsString} - {Title}.m4a";
            }

            public string NewFilePath()
            {
                return Path.Combine(File.DirectoryName, NewFileName());
            }

            public string Description()
            {
                return $"{Album}/{NewFileName()}";
            }

            public void Rename()
            {
                var newFilePath = NewFilePath();
                System.IO.File.Move(File.FullName, newFilePath);
            }

            public RenameTarget(FileInfo file, long trackId, string album, string title)
            {
                File = file;
                TrackId = trackId;
                Album = album;
                Title = title;
            }
        }

        IReadOnlyList<RenameTarget> Targets()
        {
            return
                Environment.GetCommandLineArgs()
                .SelectMany(FileIterator)
                .Where(file => file.Extension == ".m4a")
                .Distinct()
                .AsParallel()
                .Select(file =>
                {
                    using (var musicFile = TagLib.File.Create(file.FullName))
                    {
                        var tag = musicFile.GetTag(TagLib.TagTypes.Apple);
                        return new RenameTarget(file, tag.Track, tag.Album, Sanitize(tag.Title));
                    }
                })
                .OrderBy(r => r.Description())
                .ToArray();
        }

        public void Run()
        {
            var targets = Targets();

            foreach (var target in targets)
            {
                Console.WriteLine(target.Description());
            }

            Console.WriteLine("OK? (Y/n)");
            if (Console.ReadLine() == "Y")
            {
                foreach (var target in targets)
                {
                    target.Rename();
                }
            }
        }

        public static void Main(string[] args)
        {
            new Program().Run();
        }
    }
}
