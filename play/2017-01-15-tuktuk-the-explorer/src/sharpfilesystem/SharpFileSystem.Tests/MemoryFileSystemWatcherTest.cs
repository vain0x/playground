using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NUnit.Framework;
using SharpFileSystem.FileSystems;

namespace SharpFileSystem.Tests
{
    [TestFixture]
    public class MemoryFileSystemWatcherTestBase
    {
        protected MemoryFileSystem FileSystem { get; set; }
        protected MemoryFileSystemWatcher Watcher { get; set; }

        [SetUp]
        public virtual void SetUp()
        {
            var path = FileSystemPath.Root.AppendDirectory("w");
            FileSystem = new MemoryFileSystem();
            FileSystem.CreateDirectory(path);
            Watcher = new MemoryFileSystemWatcher(FileSystem, path);
        }
    }

    [TestFixture]
    public class MemoryFileSystemWatcherTest_Created
        : MemoryFileSystemWatcherTestBase
    {
        List<string> Paths { get; set; }

        public override void SetUp()
        {
            base.SetUp();

            Paths = new List<string>();
            Watcher.Changed += (sender, e) =>
            {
                if (e.ChangeType == WatcherChangeTypes.Created)
                {
                    Paths.Add(e.NewPath.ToString());
                }
            };
            Watcher.EnableRaisingEvents = true;
        }

        [TearDown]
        public void TearDown()
        {
            Watcher.Dispose();
        }

        [Test]
        public void test_creating_a_file_raises()
        {
            var filePath = Watcher.Path.AppendFile("file.tmp");
            using (var stream = FileSystem.CreateFile(filePath))
            {
            }

            Assert.AreEqual(
                new[] { "/w/file.tmp" },
                Paths
            );
        }

        [Test]
        public void test_creating_a_directory_raises()
        {
            var directoryPath = Watcher.Path.AppendDirectory("dir");
            FileSystem.CreateDirectory(directoryPath);

            Assert.AreEqual(
                new[] { "/w/dir/" },
                Paths
            );
        }

        [Test]
        public void test_creating_a_directory_outside_the_watched_does_not_raise()
        {
            var directoryPath = Watcher.Path.AppendDirectory("dir");
            FileSystem.CreateDirectory(directoryPath);
            Paths.Clear();

            // Outside.
            FileSystem.CreateFile(FileSystemPath.Root.AppendFile("file.tmp"));
            FileSystem.CreateDirectory(FileSystemPath.Root.AppendDirectory("dir"));

            // Nested inside.
            FileSystem.CreateFile(directoryPath.AppendFile("file.tmp"));
            FileSystem.CreateDirectory(directoryPath.AppendDirectory("dir"));

            Assert.IsEmpty(Paths);
        }
    }
}
