using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace wpf_font_perf
{
    public sealed class TextControl
        : FrameworkElement
    {
        public static readonly DependencyProperty TextProperty = DependencyProperty.Register(
            "Text",
            typeof(string),
            typeof(TextControl),
            new FrameworkPropertyMetadata(
                "",
                FrameworkPropertyMetadataOptions.AffectsMeasure | FrameworkPropertyMetadataOptions.AffectsRender
            ));

        public string Text
        {
            get
            {
                return (string)GetValue(TextProperty);
            }
            set
            {
                SetValue(TextProperty, value);
            }
        }

        public static readonly DependencyProperty FontSizeProperty = DependencyProperty.Register(
            "FontSize",
            typeof(double),
            typeof(TextControl),
            new FrameworkPropertyMetadata(
                16.0,
                FrameworkPropertyMetadataOptions.AffectsMeasure
                | FrameworkPropertyMetadataOptions.AffectsRender
            ));

        public double FontSize
        {
            get
            {
                return (double)GetValue(FontSizeProperty);
            }
            set
            {
                SetValue(FontSizeProperty, value);
            }
        }

        string lastText;
        double lastFontSize;
        Size lastAvailableSize;
        FormattedText FormattedText;

        static readonly Typeface DefaultTypeface = new Typeface("Noto Sans CJK JP");

        static FormattedText FormatText(string text, double fontSize, double maxWidth, double maxHeight)
        {
            var formattedText = new FormattedText(
                text,
                CultureInfo.InvariantCulture,
                FlowDirection.LeftToRight,
                DefaultTypeface,
                fontSize,
                Brushes.Black
            );

            formattedText.MaxTextWidth = Math.Min(1e6, maxWidth);
            formattedText.MaxTextHeight = Math.Min(1e6, maxHeight);

            return formattedText;
        }

        static Size FormattedTextToSize(FormattedText formattedText)
        {
            return new Size(formattedText.Width, formattedText.Height);
        }

        int reuse = 0;

        protected override Size MeasureOverride(Size availableSize)
        {
            var text = Text ?? "";
            var fontSize = FontSize;

            if (FormattedText != null && text == lastText && fontSize == lastFontSize && lastAvailableSize == availableSize)
            {
                Debug.WriteLine("reuse {0}", ++reuse);
                return FormattedTextToSize(FormattedText);
            }

            FormattedText = FormatText(text, fontSize, availableSize.Width, availableSize.Height);
            lastText = text;
            lastFontSize = fontSize;
            lastAvailableSize = availableSize;

            var desiredSize = FormattedTextToSize(FormattedText);

            return desiredSize;
        }

        protected override void OnRender(DrawingContext drawingContext)
        {
            drawingContext.DrawText(FormattedText, new Point(0, 0));
        }
    }

    struct TextRange
    {
        public readonly int Start, End;

        public int Length
        {
            get
            {
                return End - Start;
            }
        }

        public TextRange(int start, int end)
        {
            Start = start;
            End = end;
        }
    }

    /// <summary>
    /// 文字列を複数行に分割するアルゴリズム
    /// </summary>
    sealed class LineSplitter
    {
        string Text;

        readonly List<TextRange> _lines = new List<TextRange>();

        public double MaxLineWidth { get; private set; }

        public IReadOnlyList<TextRange> Lines
        {
            get
            {
                return _lines;
            }
        }

        public int LineCount
        {
            get
            {
                return Lines.Count;
            }
        }

        public string GetLine(int i)
        {
            var range = Lines[i];
            return Text.Substring(range.Start, range.Length);
        }

        static bool AtCrLf(string text, int index)
        {
            return index + 1 < text.Length
                && text[index] == '\r'
                && text[index + 1] == '\n';
        }

        static void AnalyzeChar(string text, int i, double fontSize, double maxWidth, out double charWidth, out int skip)
        {
            if (i == text.Length || text[i] == '\r' || text[i] == '\n')
            {
                // 改行または終端
                charWidth = maxWidth;
                skip = AtCrLf(text, i) ? 2 : 1;
            }
            else if (text[i] < 0x20)
            {
                // 制御文字は無視
                charWidth = 0;
                skip = 0;
            }
            else if (0x20 <= text[i] && text[i] < 0x7f)
            {
                // ASCII 文字は「半角」
                charWidth = fontSize / 2;
                skip = 0;
            }
            else
            {
                // 非 ASCII 文字は「全角」 (Unicode の結合文字や絵文字や他言語は無視)
                charWidth = fontSize;
                skip = 0;
            }
        }

        public void Calculate(string text, double fontSize, double maxWidth, double maxHeight)
        {
            text = text.TrimEnd();

            if (string.IsNullOrEmpty(text)) return;

            var i = 0;
            var start = 0;
            var lineWidth = 0.0;
            var maxLineWidth = maxWidth;
            var height = 0.0;

            while (i <= text.Length && height < maxHeight)
            {
                double charWidth;
                int skip;
                AnalyzeChar(text, i, fontSize, maxWidth, out charWidth, out skip);

                var lineBreak = lineWidth + charWidth >= maxWidth;
                if (lineBreak)
                {
                    var end = i;
                    var nextIndex = end + skip;
                    var textRange = new TextRange(start, end);

                    i = nextIndex;
                    start = nextIndex;
                    _lines.Add(textRange);
                    maxLineWidth = Math.Max(maxLineWidth, lineWidth);
                    height += fontSize;
                    lineWidth = 0.0;
                    continue;
                }

                lineWidth += charWidth;
                i++;
            }

            Text = text;
            MaxLineWidth = maxLineWidth;
        }
    }
}
