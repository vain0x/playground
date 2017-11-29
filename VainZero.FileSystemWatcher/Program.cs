using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace VainZero.FileSystemWatcher
{
    public sealed class Program
    {
        readonly Dictionary<string, System.IO.FileSystemWatcher> watchers =
            new Dictionary<string, System.IO.FileSystemWatcher>();

        readonly ConcurrentQueue<string> writeQueue =
            new ConcurrentQueue<string>();

        readonly AutoResetEvent resetEvent =
            new AutoResetEvent(initialState: false);

        void OnFileSystemChanged(object sender, FileSystemEventArgs e)
        {
            writeQueue.Enqueue($"{e.ChangeType}|{e.FullPath}");
            resetEvent.Set();
        }

        void OnRenamed(object sender, RenamedEventArgs e)
        {
            writeQueue.Enqueue($"Renamed|{e.OldFullPath}|{e.FullPath}");
            resetEvent.Set();
        }

        void Attach(System.IO.FileSystemWatcher watcher)
        {
            watcher.Changed += OnFileSystemChanged;
            watcher.Created += OnFileSystemChanged;
            watcher.Deleted += OnFileSystemChanged;
            watcher.Renamed += OnRenamed;
            watcher.EnableRaisingEvents = true;
        }

        void Detach(System.IO.FileSystemWatcher watcher)
        {
            watcher.Changed -= OnFileSystemChanged;
            watcher.Created -= OnFileSystemChanged;
            watcher.Deleted -= OnFileSystemChanged;
            watcher.Renamed -= OnRenamed;
        }

        async Task ReadAsync()
        {
            while (true)
            {
                var line = await Console.In.ReadLineAsync();
                if (line == null) return;

                if (line.StartsWith("Add|"))
                {
                    var directoryPath = Path.GetFullPath(line.Substring(4));
                    if (!Directory.Exists(directoryPath)) continue;
                    if (watchers.ContainsKey(directoryPath)) continue;

                    var watcher = new System.IO.FileSystemWatcher(directoryPath);
                    watchers.Add(directoryPath, watcher);
                    Attach(watcher);
                }
                else if (line.StartsWith("Remove|"))
                {
                    var directoryPath = Path.GetFullPath(line.Substring(7));
                    var watcher = default(System.IO.FileSystemWatcher);
                    if (watchers.TryGetValue(directoryPath, out watcher))
                    {
                        Detach(watcher);
                        watchers.Remove(directoryPath);
                    }
                }
            }
        }

        void Write()
        {
            while (resetEvent.WaitOne())
            {
                resetEvent.Reset();

                var line = default(string);
                if (writeQueue.TryDequeue(out line))
                {
                    Console.WriteLine(line);
                }
            }
        }

        void Run()
        {
            var readTask = ReadAsync();
            var writeTask = Task.Run(() => Write());
            Task.WhenAll(readTask, writeTask).Wait();
        }

        public static void Main(string[] args)
        {
            (new Program()).Run();
        }
    }
}
