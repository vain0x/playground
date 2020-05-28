using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Markup;
using System.Windows.Media;

namespace DotNetKit.Windows.Controls
{
    [ContentProperty("Text")]
    public class TextBlockSlim
        : FrameworkElement
    {
        static readonly FontFamily FallbackFontFamily =
            (FontFamily)Control.FontFamilyProperty.DefaultMetadata.DefaultValue;

        #region Text
        public static readonly DependencyProperty TextProperty =
            DependencyProperty.Register(
                "Text",
                typeof(string),
                typeof(TextBlockSlim)
            );

        public string Text
        {
            get { return (string)GetValue(TextProperty); }
            set { SetValue(TextProperty, value); }
        }
        #endregion

        #region FontFamily
        public static readonly DependencyProperty FontFamilyProperty =
            DependencyProperty.Register(
                "FontFamily",
                typeof(FontFamily),
                typeof(TextBlockSlim),
                new PropertyMetadata(FallbackFontFamily)
            );

        public FontFamily FontFamily
        {
            get { return (FontFamily)GetValue(FontFamilyProperty); }
            set { SetValue(FontFamilyProperty, value); }
        }
        #endregion

        #region FontStyle
        public static readonly DependencyProperty FontStyleProperty =
            DependencyProperty.Register(
                "FontStyle",
                typeof(FontStyle),
                typeof(TextBlockSlim),
                new PropertyMetadata(FontStyles.Normal)
            );

        public FontStyle FontStyle
        {
            get { return (FontStyle)GetValue(FontStyleProperty); }
            set { SetValue(FontStyleProperty, value); }
        }
        #endregion

        #region FontWeight
        public static readonly DependencyProperty FontWeightProperty =
            DependencyProperty.Register(
                "FontWeight",
                typeof(FontWeight),
                typeof(TextBlockSlim),
                new PropertyMetadata(FontWeights.Normal)
            );

        public FontWeight FontWeight
        {
            get { return (FontWeight)GetValue(FontWeightProperty); }
            set { SetValue(FontWeightProperty, value); }
        }
        #endregion

        #region FontStretch
        public static readonly DependencyProperty FontStretchProperty =
            DependencyProperty.Register(
                "FontStretch",
                typeof(FontStretch),
                typeof(TextBlockSlim),
                new PropertyMetadata(FontStretches.Normal)
            );

        public FontStretch FontStretch
        {
            get { return (FontStretch)GetValue(FontStretchProperty); }
            set { SetValue(FontStretchProperty, value); }
        }
        #endregion

        #region FontSize
        public static readonly DependencyProperty FontSizeProperty =
            DependencyProperty.Register(
                "FontSize",
                typeof(double),
                typeof(TextBlockSlim),
                new PropertyMetadata(12.0)
            );

        public double FontSize
        {
            get { return (double)GetValue(FontSizeProperty); }
            set { SetValue(FontSizeProperty, value); }
        }
        #endregion

        #region Foreground
        public static readonly DependencyProperty ForegroundProperty =
            DependencyProperty.Register(
                "Foreground",
                typeof(Brush),
                typeof(TextBlockSlim),
                new PropertyMetadata(Brushes.Black)
            );

        public Brush Foreground
        {
            get { return (Brush)GetValue(ForegroundProperty); }
            set { SetValue(ForegroundProperty, value); }
        }
        #endregion

        #region TextWrapping
        public static readonly DependencyProperty TextWrappingProperty =
            DependencyProperty.Register(
                "TextWrapping",
                typeof(TextWrapping),
                typeof(TextBlockSlim),
                new PropertyMetadata(TextWrapping.NoWrap)
            );

        public TextWrapping TextWrapping
        {
            get { return (TextWrapping)GetValue(TextWrappingProperty); }
            set { SetValue(TextWrappingProperty, value); }
        }
        #endregion

        FormattedText FormattedText(Size constraint)
        {
            var typeface =
                new Typeface(
                    FontFamily,
                    FontStyle,
                    FontWeight,
                    FontStretch,
                    FallbackFontFamily
                );
            var ft =
                new FormattedText(
                    Text ?? "",
                    CultureInfo.CurrentUICulture,
                    GetFlowDirection(this),
                    typeface,
                    FontSize,
                    Foreground
                );

            if (!double.IsPositiveInfinity(constraint.Width) && constraint.Width > 0)
            {
                ft.MaxTextWidth = constraint.Width;
            }

            if (!double.IsPositiveInfinity(constraint.Height) && constraint.Height > 0)
            {
                ft.MaxTextHeight = constraint.Height;
            }

            ft.TextAlignment = TextBlock.GetTextAlignment(this);
            return ft;
        }

        protected override Size MeasureOverride(Size constraint)
        {
            var formattedText = 
                FormattedText(
                    TextWrapping == TextWrapping.NoWrap
                        ? new Size(double.PositiveInfinity, constraint.Height)
                        : constraint
                );
            return
                new Size(
                    Math.Min(constraint.Width, formattedText.Width),
                    Math.Min(constraint.Height, formattedText.Height)
                );
        }

        protected override Size ArrangeOverride(Size arrangeBounds)
        {
            return arrangeBounds;
        }

        protected override void OnRender(DrawingContext drawingContext)
        {
            var formattedText = FormattedText(new Size(ActualWidth, ActualHeight));
            drawingContext.DrawText(formattedText, new Point(0, 0));
        }
    }
}
