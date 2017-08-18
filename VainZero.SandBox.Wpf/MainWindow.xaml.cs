using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reactive.Concurrency;
using System.Reactive.Disposables;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reactive.Threading.Tasks;
using System.Text;
using System.Threading;
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
using Reactive.Bindings;

namespace VainZero.SandBox.Wpf
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            var grid =
                UniformGrid.Create(
                    7,
                    10,
                    (y, x) =>
                        "いろはにほへとちりぬるをわかよたれそつねならむうゐのおくやまけふこえてあさきゆめみしゑひもせすいろはにほへとちりぬるをわかよたれそつねならむうゐのおくやまけふこえてあさきゆめみしゑひもせす"
                );
            DataContext = grid;

            var size = new Size(1024, 768);

            var stopwatch = Stopwatch.StartNew();
            var timestamps = new List<double>() { 0.0 };
            foreach (var _ in Enumerable.Range(0, 10))
            {
                var ui = new MyControl() { DataContext = grid };
                ui.Measure(size);
                ui.Arrange(new Rect(new Point(0, 0), size));
                ui.UpdateLayout();
                timestamps.Add(stopwatch.Elapsed.TotalMilliseconds);
            }
            stopwatch.Stop();

            var times =
                Enumerable.Range(0, timestamps.Count - 1)
                .Select(i => timestamps[i + 1] - timestamps[i])
                .Skip(1)
                .ToArray();

            var v = 0.0;
            var average = times.Average();
            foreach (var time in times)
            {
                v += Math.Sqrt((time - average) * (time - average));
                Debug.WriteLine(time.ToString("N3") + "ms");
            }

            Debug.WriteLine($"Average: {average.ToString("N3")}ms");
            Debug.WriteLine($"Variance: {v / times.Length}");

            // About 600ms.
        }
    }

    public sealed class UniformGrid
    {
        public int RowCount { get; }
        public int ColumnCount { get; }
        public IReadOnlyList<Cell> Cells { get; }

        public UniformGrid(int rowCount, int columnCount, IReadOnlyList<Cell> cells)
        {
            RowCount = rowCount;
            ColumnCount = columnCount;
            Cells = cells;
        }

        public static UniformGrid Create(int rowCount, int columnCount, Func<int, int, string> func)
        {
            var cells =
                Enumerable.Range(0, rowCount * columnCount)
                .Select(i =>
                    new Cell(
                        i / columnCount,
                        i % columnCount,
                        func(i / columnCount, i % columnCount)
                    ))
                .ToArray();
            return new UniformGrid(rowCount, columnCount, cells);
        }
    }

    public sealed class Cell
    {
        public int RowIndex { get; }
        public int ColumnIndex { get; }
        public string Text { get; }

        public Cell(int rowIndex, int columnIndex, string text)
        {
            RowIndex = rowIndex;
            ColumnIndex = columnIndex;
            Text = text;
        }
    }
}
