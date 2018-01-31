using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Sandbox
{
    public sealed class Program
    {
        void OnFile(FileInfo file)
        {
            if (file.Extension == ".exe") return;

            var content = File.ReadAllText(file.FullName);
            var newContent =
                content
                .Replace("Reactive.Behaviors", "Reactive.Bindables")
                .Replace("Behavior", "Bindable")
                .Replace("behavior", "bindable")
                .Replace("ビヘイビアー", "バインダブル");
            if (content != newContent)
            {
                File.WriteAllText(file.FullName, newContent);
            }

            var name = file.Name;
            var newName = name.Replace("Behavior", "Bindable");
            if (name != newName)
            {
                file.MoveTo(Path.Combine(file.Directory.FullName, newName));
            }
        }

        void OnDirectory(DirectoryInfo dir)
        {
            foreach (var file in dir.EnumerateFiles())
            {
                OnFile(file);
            }

            foreach (var subdir in dir.EnumerateDirectories())
            {
                if (new[] { ".git", "documents", "bin", "obj" }.Contains(subdir.Name)) continue;

                OnDirectory(subdir);

                var name = subdir.Name;
                var newName = name.Replace("Behavior", "Bindable");
                if (name != newName)
                {
                    subdir.MoveTo(Path.Combine(subdir.Parent.FullName, newName));
                }
            }
        }

        public void Run()
        {
            var root = new DirectoryInfo(@"C:\repo\DotNetKit.Reactive");
            OnDirectory(root);
        }

        public static void Main(string[] args)
        {
            new Program().Run();
        }
    }
}
