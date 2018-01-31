using System;
using System.Collections.Generic;
using System.ComponentModel;
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
using Prism.Commands;
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

            Main =
                new ReactiveProperty<MainView>(
                    new MainView(
                        new DelegateCommand(() =>
                        {
                            Main.Value =
                                Main.Value.WithItems(
                                    Main.Value.Items.Concat(new[] { new Item("hello") }).ToArray()
                                );
                        }),
                        new Item[0]
                    ));

            Main.Subscribe(main =>
            {
                for (var i = 0; i < main.Items.Count; i++)
                {
                    var index = i;
                    var item = main.Items[i];
                    item.TextChanged += (sender, text) =>
                    {
                        Main.Value = main.WithItems(main.Items.WithItem(index, item.WithText(text)));
                    };
                }
            });

            DataContext = Main;
        }

        public ReactiveProperty<MainView> Main { get; }
    }

    public static class ListExtension
    {
        public static IReadOnlyList<X> WithItem<X>(this IReadOnlyList<X> @this, int index, X value)
        {
            return @this.Take(index).Concat(new[] { value }).Concat(@this.Skip(index + 1)).ToArray();
        }
    }

    public sealed class Item
    {
        public event EventHandler<string> TextChanged;

        readonly string text;
        public string Text
        {
            get { return text; }
            set { TextChanged?.Invoke(this, value); }
        }

        public Item WithText(string text)
        {
            return new Item(text);
        }

        public override string ToString()
        {
            return $"{{Text = {text}}}";
        }

        // analyzer: complete-constructor
        public Item(string text)
        {
            if (text == null)
                throw new ArgumentNullException(nameof(text));
            this.text = text;
        }
    }

    public sealed class MainView
    {
        public DelegateCommand AddCommand { get; }
        public IReadOnlyList<Item> Items { get; }

        public MainView WithItems(IReadOnlyList<Item> items)
        {
            return new MainView(AddCommand, items);
        }

        public override string ToString()
        {
            return $"{{Items = {Items.Count}}}";
        }

        // analyzer: complete-constructor
        public MainView(DelegateCommand addCommand, IReadOnlyList<Item> items)
        {
            if (addCommand == null)
                throw new ArgumentNullException(nameof(addCommand));
            if (items == null)
                throw new ArgumentNullException(nameof(items));
            AddCommand = addCommand;
            Items = items;
        }
    }
}
