using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
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
using System.Windows.Threading;

namespace DotNetKit.Windows.Controls
{
    /// <summary>
    /// LazyItemsControl.xaml の相互作用ロジック
    /// </summary>
    public partial class LazyItemsControl : ItemsControl
    {
        sealed class LazyObservableList
            : ObservableCollection<object>
        {
            public LazyObservableList(Func<IEnumerable> nextItems)
            {
                this.nextItems = nextItems;
            }

            readonly Func<IEnumerable> nextItems;

            bool isTerminated;

            public void AddNextItems()
            {
                if (isTerminated) return;

                var items = nextItems();
                var isEmpty = true;

                foreach (var item in items)
                {
                    isEmpty = false;
                    Add(item);
                }

                if (isEmpty)
                {
                    isTerminated = true;
                }
            }
        }

        public LazyItemsControl()
        {
            InitializeComponent();
        }

        #region NextItems
        public static readonly DependencyProperty NextItemsProperty =
            DependencyProperty.Register(
                "NextItems",
                typeof(Func<IEnumerable>),
                typeof(LazyItemsControl),
                new FrameworkPropertyMetadata()
                {
                    PropertyChangedCallback = OnNextItemsPropertyChanged,
                }
            );

        public Func<IEnumerable> NextItems
        {
            get { return (Func<IEnumerable>)GetValue(NextItemsProperty); }
            set { SetValue(NextItemsProperty, value); }
        }

        static void OnNextItemsPropertyChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            var @this = (LazyItemsControl)sender;

            var itemsSource = new LazyObservableList((Func<IEnumerable>)e.NewValue);
            itemsSource.AddNextItems();

            @this.ItemsSource = itemsSource;
        }
        #endregion

        void PART_ScrollViewer_ScrollChanged(object sender, ScrollChangedEventArgs e)
        {
            // Add new items until scrollable area is filled.
            var hasRoom = e.ViewportHeight >= e.ExtentHeight;

            // Add new items whenever scrolled to the bottom.
            var hitsBottom = Math.Abs((e.VerticalOffset + e.ViewportHeight) - e.ExtentHeight) < 0.01;

            if (hasRoom || hitsBottom)
            {
                var itemsSource = ItemsSource as LazyObservableList;
                if (itemsSource != null)
                {
                    itemsSource.AddNextItems();
                }
            }
        }
    }
}
