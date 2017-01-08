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
    public sealed class SimpleToastNotification
        : ToastNotification
    {
        public static Duration DefaultFadeDuration { get; set; } =
            new Duration(TimeSpan.FromSeconds(8.0));

        public override Duration FadeDuration => DefaultFadeDuration;

        public SimpleToastNotificationTheme Theme { get; }

        public string Title { get; set; } =
            "";

        public string Message { get; set; } =
            "";

        public ICommand Command { get; set; } =
            AlwaysExecutableCommand.Empty;

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
