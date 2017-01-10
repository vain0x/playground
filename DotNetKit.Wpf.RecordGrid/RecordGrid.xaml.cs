﻿using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Markup;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace DotNetKit.Wpf
{
    /// <summary>
    /// RecordGrid.xaml の相互作用ロジック
    /// </summary>
    [ContentProperty("Children")]
    public partial class RecordGrid
        : UserControl
    {
        public Orientation Orientation { get; set; }

        public Collection<UIElement> Children { get; private set; }

        #region IsLabel
        static readonly DependencyProperty isLabelProperty =
            DependencyProperty.RegisterAttached(
                "IsLabel",
                typeof(bool),
                typeof(RecordGrid)
            );

        public static DependencyProperty IsLabelProperty
        {
            get { return isLabelProperty; }
        }

        public static bool GetIsLabel(DependencyObject obj)
        {
            return (bool)obj.GetValue(IsLabelProperty);
        }

        public static void SetIsLabel(DependencyObject obj, bool value)
        {
            obj.SetValue(IsLabelProperty, value);
        }
        #endregion

        #region IsOdd
        static readonly DependencyProperty isOddProperty =
            DependencyProperty.RegisterAttached(
                "IsOdd",
                typeof(bool),
                typeof(RecordGrid)
            );

        public static DependencyProperty IsOddProperty
        {
            get { return isOddProperty; }
        }

        public static bool GetIsOdd(DependencyObject obj)
        {
            return (bool)obj.GetValue(IsOddProperty);
        }

        public static void SetIsOdd(DependencyObject obj, bool value)
        {
            obj.SetValue(IsOddProperty, value);
        }
        #endregion

        DependencyProperty GridRowProperty
        {
            get { return Orientation == Orientation.Vertical ? Grid.RowProperty : Grid.ColumnProperty; }
        }

        DependencyProperty GridColumnProperty
        {
            get { return Orientation == Orientation.Vertical ? Grid.ColumnProperty : Grid.RowProperty; }
        }

        void AddGridRow()
        {
            if (Orientation == Orientation.Vertical)
            {
                grid.RowDefinitions.Add(new RowDefinition() { Height = GridLength.Auto });
            }
            else
            {
                grid.ColumnDefinitions.Add(new ColumnDefinition() { Width = GridLength.Auto });
            }
        }

        void AddGridColumn(GridLength length)
        {
            if (Orientation == Orientation.Vertical)
            {
                grid.ColumnDefinitions.Add(new ColumnDefinition() { Width = length });
            }
            else
            {
                grid.RowDefinitions.Add(new RowDefinition() { Height = length });
            }
        }

        void CreateColumnDefinitions()
        {
            AddGridColumn(GridLength.Auto);
            AddGridColumn(new GridLength(1.0, GridUnitType.Star));
        }

        void AddUIElement(int index, UIElement element)
        {
            var rowIndex = index / 2;
            var columnIndex = index % 2;
            var isOdd = rowIndex % 2 != 0;

            if (columnIndex == 0)
            {
                AddGridRow();
            }

            element.SetValue(GridRowProperty, rowIndex);
            element.SetValue(GridColumnProperty, columnIndex);
            SetIsLabel(element, columnIndex == 0);
            SetIsOdd(element, isOdd);
            grid.Children.Add(element);
        }

        void Reset()
        {
            CreateColumnDefinitions();

            var index = 0;
            foreach (var item in Children)
            {
                AddUIElement(index, item);
                index++;
            }
        }

        public override void OnApplyTemplate()
        {
            base.OnApplyTemplate();
            Reset();
        }

        public RecordGrid()
        {
            Orientation = Orientation.Vertical;
            Children = new Collection<UIElement>();

            InitializeComponent();
        }
    }
}
