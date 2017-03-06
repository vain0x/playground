using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using DotNetKit.Functional.Commands;

namespace DotNetKit.Windows.Controls
{
    /// <summary>
    /// Reprsents a simple toast notification, which contains an icon, title, and a message.
    /// </summary>
    public sealed class SimpleToastNotification
        : ToastNotification
    {
        /// <summary>
        /// Gets or sets the default duration until beginning to fade out.
        /// </summary>
        public static TimeSpan? DefaultDuration { get; set; } =
            TimeSpan.FromSeconds(5.0);

        /// <summary>
        /// Gets the default duration to fade out.
        /// <c>null</c> for forever.
        /// </summary>
        public static Duration DefaultFadeDuration { get; set; } =
            new Duration(TimeSpan.FromSeconds(1.0));

        /// <summary>
        /// Gets or sets the default width.
        /// </summary>
        public static double DefaultWidth { get; set; } =
            250.0;

        /// <summary>
        /// Gets the duration until beginning to fade out.
        /// </summary>
        public override TimeSpan? Duration => DefaultDuration;

        /// <summary>
        /// Gets the duration to fade out.
        /// </summary>
        public override Duration FadeDuration => DefaultFadeDuration;

        /// <summary>
        /// Gets the theme.
        /// </summary>
        public SimpleToastNotificationTheme Theme { get; }

        /// <summary>
        /// Gets or sets the title.
        /// </summary>
        public string Title { get; set; } =
            "";

        /// <summary>
        /// Gets or sets the message.
        /// </summary>
        public string Message { get; set; } =
            "";

        /// <summary>
        /// Gets or sets the command, which is executed when clicked or tapped.
        /// </summary>
        public ICommand Command { get; set; } =
            AlwaysExecutableCommand.Empty;

        /// <summary>
        /// Gets or sets the width.
        /// </summary>
        public double Width { get; set; } =
            DefaultWidth;

        /// <summary>
        /// Constructs an instance.
        /// </summary>
        /// <param name="theme"></param>
        public SimpleToastNotification(SimpleToastNotificationTheme theme)
        {
            Theme = theme;
        }

        /// <summary>
        /// Constructs an instance, which represents an information message.
        /// </summary>
        public SimpleToastNotification()
            : this(SimpleToastNotificationTheme.Info)
        {
        }
    }
}
