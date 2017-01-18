using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using NUnit.Framework;
using SharpFileSystem.FileSystems;

namespace SharpFileSystem.Tests
{
    [TestFixture]
    public class PhysicalFileSystemWatcherTestBase
    {
        protected string Root { get; set; }
        protected PhysicalFileSystem FileSystem { get; set; }
        protected AutoResetEvent ResetEvent { get; set; }

        [SetUp]
        public virtual void Initialize()
        {
            Root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
            System.IO.Directory.CreateDirectory(Root);
            FileSystem = new PhysicalFileSystem(Root);
            ResetEvent = new AutoResetEvent(false);
        }

        [TearDown]
        public virtual void Cleanup()
        {
            FileSystem.Dispose();
            System.IO.Directory.Delete(Root, true);
        }

        protected void WaitNext()
        {
            var signal = ResetEvent.WaitOne(TimeSpan.FromSeconds(20.0));
            Assert.IsTrue(signal);
        }

        protected void WaitTimeout()
        {
            var signal = ResetEvent.WaitOne(TimeSpan.FromSeconds(5.0));
            Assert.IsFalse(signal);
        }
    }

    [TestFixture]
    public class PhysicalFileSystemWatcherTest_Created
        : PhysicalFileSystemWatcherTestBase
    {
        List<string> Paths { get; set; }
        PhysicalFileSystemWatcher Watcher { get; set; }

        public override void Initialize()
        {
            base.Initialize();

            Paths = new List<string>();
            Watcher = new PhysicalFileSystemWatcher(FileSystem, FileSystemPath.Root);
            Watcher.Created += (sender, e) =>
            {
                Paths.Add(e.FullPath);
                ResetEvent.Set();
            };
            Watcher.EnableRaisingEvents = true;
        }

        public override void Cleanup()
        {
            Watcher.Dispose();
            base.Cleanup();
        }

        [Test]
        public void test_creating_a_file_raises_Created()
        {
            var filePath = FileSystemPath.Root.AppendFile("file.tmp");
            using (var stream = FileSystem.CreateFile(filePath))
            {
            }

            WaitNext();
            Assert.AreEqual(
                new[] { Path.Combine(Root, filePath.EntityName) },
                Paths
            );
        }

        [Test]
        public void test_creating_a_directory_raises_Created()
        {
            var directoryPath = FileSystemPath.Root.AppendDirectory("dir");
            FileSystem.CreateDirectory(directoryPath);

            WaitNext();
            Assert.AreEqual(
                new[] { Path.Combine(Root, directoryPath.EntityName) },
                Paths
            );
        }
    }

    [TestFixture]
    public class PhysicalFileSystemWatcherTest_Changed
        : PhysicalFileSystemWatcherTestBase
    {
        PhysicalFileSystemWatcher Watcher { get; set; }
        int Count { get; set; }

        public override void Initialize()
        {
            base.Initialize();

            Watcher = new PhysicalFileSystemWatcher(FileSystem, FileSystemPath.Root);
            Watcher.Changed += (sender, e) =>
            {
                Count++;
                ResetEvent.Set();
            };
            Watcher.EnableRaisingEvents = true;
        }

        public override void Cleanup()
        {
            Watcher.Dispose();
            base.Cleanup();
        }

        [Test]
        public void test_writing_to_a_file_raises_Changed()
        {
            var filePath = FileSystemPath.Root.AppendFile("file.tmp");
            using (var stream = FileSystem.CreateFile(filePath))
            {
                stream.WriteByte(0);
            }

            WaitNext();
            Assert.IsTrue(Count > 0);
        }
    }

    [TestFixture]
    public class PhysicalFileSystemWatcherTest_Deleted
        : PhysicalFileSystemWatcherTestBase
    {
        List<string> Paths { get; set; }
        PhysicalFileSystemWatcher Watcher { get; set; }

        public override void Initialize()
        {
            base.Initialize();

            Paths = new List<string>();
            Watcher = new PhysicalFileSystemWatcher(FileSystem, FileSystemPath.Root);
            Watcher.Deleted += (sender, e) =>
            {
                Paths.Add(e.FullPath);
                ResetEvent.Set();
            };
            Watcher.EnableRaisingEvents = true;
        }

        public override void Cleanup()
        {
            Watcher.Dispose();
            base.Cleanup();
        }

        [Test]
        public void test_removing_a_directory_raises_Removed()
        {
            var directoryPath = FileSystemPath.Root.AppendDirectory("dir");
            FileSystem.CreateDirectory(directoryPath);
            FileSystem.Delete(directoryPath);

            WaitNext();
            Assert.AreEqual(
                new[] { Path.Combine(Root, directoryPath.EntityName) },
                Paths
            );
        }

        [Test]
        public void test_removing_a_file_raises_Created()
        {
            var filePath = FileSystemPath.Root.AppendFile("file.tmp");
            using (var stream = FileSystem.CreateFile(filePath))
            {
            }
            FileSystem.Delete(filePath);

            WaitNext();
            Assert.AreEqual(
                new[] { Path.Combine(Root, filePath.EntityName) },
                Paths
            );
        }
    }
}
