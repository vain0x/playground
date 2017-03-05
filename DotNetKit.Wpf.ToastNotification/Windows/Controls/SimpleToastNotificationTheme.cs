using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;

namespace DotNetKit.Windows.Controls
{
    /// <summary>
    /// Represents a theme of <see cref="SimpleToastNotification"/>.
    /// </summary>
    public sealed class SimpleToastNotificationTheme
    {
        /// <summary>
        /// Gets the foreground.
        /// </summary>
        public Brush Foreground { get; }

        /// <summary>
        /// Gets the background.
        /// </summary>
        public Brush Background { get; }

        /// <summary>
        /// Gets the icon.
        /// </summary>
        public Geometry Icon { get; }

        /// <summary>
        /// Gets the font size of title.
        /// </summary>
        public double TitleFontSize { get; }

        /// <summary>
        /// Gets the font size of message.
        /// </summary>
        public double MessageFontSize { get; }

        /// <summary>
        /// Constructs an instance.
        /// </summary>
        /// <param name="foregound"></param>
        /// <param name="background"></param>
        /// <param name="icon"></param>
        /// <param name="titleFontSize"></param>
        /// <param name="messageFontSize"></param>
        public
            SimpleToastNotificationTheme(
                Brush foregound,
                Brush background,
                Geometry icon,
                double titleFontSize,
                double messageFontSize
            )
        {
            Foreground = foregound;
            Background = background;
            Icon = icon;
            TitleFontSize = titleFontSize;
            MessageFontSize = messageFontSize;
        }

        static SimpleToastNotificationTheme Create(Color backgroundColor, string iconPath)
        {
            return
                new SimpleToastNotificationTheme(
                    Brushes.White,
                    new SolidColorBrush(backgroundColor),
                    Geometry.Parse(iconPath),
                    18,
                    14
                );
        }

        // Theme icons are from <https://github.com/zachatrocity/netoaster>.
        // Copyright (c) 2015 zachatrocity

        /// <summary>
        /// Gets a theme for successful notifications.
        /// </summary>
        public static SimpleToastNotificationTheme Success { get; } =
            Create(
                Color.FromRgb(0x4B, 0xA2, 0x53),
                "F1 M23.7501 37.25 L34.8334 48.3333 L52.2499 26.1668 L56.9999 30.9168 L34.8334 57.8333 L19.0001 42 L23.7501 37.25 Z "
            );

        /// <summary>
        /// Gets a theme for informational notifications.
        /// </summary>
        public static SimpleToastNotificationTheme Info { get; } =
            Create(
                Color.FromRgb(0x5E, 0xAE, 0xC5),
                "F1 M 38,19C 48.4934,19 57,27.5066 57,38C 57,48.4934 48.4934,57 38,57C 27.5066,57 19,48.4934 19,38C 19,27.5066 27.5066,19 38,19 Z M 33.25,33.25L 33.25,36.4167L 36.4166,36.4167L 36.4166,47.5L 33.25,47.5L 33.25,50.6667L 44.3333,50.6667L 44.3333,47.5L 41.1666,47.5L 41.1666,36.4167L 41.1666,33.25L 33.25,33.25 Z M 38.7917,25.3333C 37.48,25.3333 36.4167,26.3967 36.4167,27.7083C 36.4167,29.02 37.48,30.0833 38.7917,30.0833C 40.1033,30.0833 41.1667,29.02 41.1667,27.7083C 41.1667,26.3967 40.1033,25.3333 38.7917,25.3333 Z "
            );

        /// <summary>
        /// Gets a theme for error notifications.
        /// </summary>
        public static SimpleToastNotificationTheme Error { get; } =
            Create(
                Color.FromRgb(0xC4, 0x38, 0x29),
                "F1 M 31.6667,19L 44.3333,19L 57,31.6667L 57,44.3333L 44.3333,57L 31.6667,57L 19,44.3333L 19,31.6667L 31.6667,19 Z M 26.4762,45.0454L 30.9546,49.5238L 38,42.4783L 45.0454,49.5238L 49.5237,45.0454L 42.4783,38L 49.5238,30.9546L 45.0454,26.4763L 38,33.5217L 30.9546,26.4762L 26.4762,30.9546L 33.5217,38L 26.4762,45.0454 Z "
            );

        /// <summary>
        /// Gets a theme for warning notifications.
        /// </summary>
        public static SimpleToastNotificationTheme Warning { get; } =
            Create(
                Color.FromRgb(0xFF, 0x94, 0x00),
                "F1 M 38,19C 48.4934,19 57,27.5066 57,38C 57,48.4934 48.4934,57 38,57C 27.5066,57 19,48.4934 19,38C 19,27.5066 27.5066,19 38,19 Z M 34.0417,25.7292L 36.0208,41.9584L 39.9792,41.9583L 41.9583,25.7292L 34.0417,25.7292 Z M 38,44.3333C 36.2511,44.3333 34.8333,45.7511 34.8333,47.5C 34.8333,49.2489 36.2511,50.6667 38,50.6667C 39.7489,50.6667 41.1667,49.2489 41.1667,47.5C 41.1667,45.7511 39.7489,44.3333 38,44.3333 Z "
            );
    }
}
