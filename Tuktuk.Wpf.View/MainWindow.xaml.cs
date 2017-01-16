using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using SharpFileSystem;
using SharpFileSystem.FileSystems;
using Tuktuk.Wpf.Controls;

namespace Tuktuk.Wpf
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        Shelve CreateInitialShelve()
        {
            var fileSystem = PhysicalFileSystem.SuperRoot;
            var drives = fileSystem.GetEntities(FileSystemPath.Root);
            var books =
                drives.Select(drive =>
                {
                    var paths =
                        new[]
                        {
                            drive,
                            fileSystem.GetVirtualDirectoryPath(Environment.CurrentDirectory),
                        };
                    var pages =
                        paths.Select(path => new Controls.Page(fileSystem, path));
                    return new Book(fileSystem, $"Book {drive.EntityName}", pages);
                }).ToArray();
            var workspaces =
                new[]
                {
                    new Workspace(books[0].ActivePage.Value),
                    new Workspace(books[1].ActivePage.Value),
                };
            var shelve = new Shelve(fileSystem, books, workspaces);
            return shelve;
        }

        public MainWindow()
        {
            InitializeComponent();

            DataContext =
                new
                {
                    Shelve = CreateInitialShelve(),
                };
        }
    }
}
