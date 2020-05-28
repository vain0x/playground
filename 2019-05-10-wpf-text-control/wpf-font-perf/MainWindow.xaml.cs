using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows;

namespace wpf_font_perf
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            // new FontInstaller().Install();

            DataContext = new
            {
                Items = Enumerable.Range(0, 16).Select(i => new
                {
                    Row = i % 4,
                    Column = i / 4,
                    Text = string.Join("", Enumerable.Range(0, 30).Select(_ => "Iroha に hohe とちりぬるを")),
                }),
            };
        }
    }

    sealed class FontInstaller
    {
        public void Install()
        {
            var fileName = "NotoSansCJKjp-Regular.otf";
            var fontDir = Environment.GetFolderPath(Environment.SpecialFolder.Fonts);
            var destPath = Path.Combine(fontDir, fileName);
            var sourcePath = Path.Combine("./Font", fileName);

            if (!File.Exists(destPath))
            {
                File.Copy(sourcePath, destPath, overwrite: true);
            }
        }
    }
}
