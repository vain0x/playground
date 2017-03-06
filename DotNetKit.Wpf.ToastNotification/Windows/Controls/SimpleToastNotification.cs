using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.CompilerServices;
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
        , INotifyPropertyChanged
    {
        /// <summary>
        /// Occurs when one of properties changed.
        /// </summary>
        public event PropertyChangedEventHandler PropertyChanged;

        void RaisePropertyChanged([CallerMemberName] string propertyName = default(string))
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        /// <summary>
        /// Gets or sets the default width.
        /// </summary>
        public static double DefaultWidth { get; set; } =
            250.0;

        /// <summary>
        /// Gets the duration until beginning to fade out.
        /// </summary>
        public override TimeSpan? Duration =>
            TimeSpan.FromSeconds(5.0);

        /// <summary>
        /// Gets the duration to fade out.
        /// </summary>
        public override Duration FadeDuration =>
            new Duration(TimeSpan.FromSeconds(1.0));

        /// <summary>
        /// Gets the theme.
        /// </summary>
        public SimpleToastNotificationTheme Theme { get; }

        string title = "";

        /// <summary>
        /// Gets or sets the title.
        /// </summary>
        public string Title
        {
            get { return title; }
            set { title = value; RaisePropertyChanged(); }
        }

        string message = "";

        /// <summary>
        /// Gets or sets the message.
        /// </summary>
        public string Message
        {
            get { return message; }
            set { message = value; RaisePropertyChanged(); }
        }

        ICommand command = AlwaysExecutableCommand.Empty;

        /// <summary>
        /// Gets or sets the command, which is executed when clicked or tapped.
        /// </summary>
        public ICommand Command
        {
            get { return command; }
            set { command = value; RaisePropertyChanged(); }
        }

        double width = DefaultWidth;

        /// <summary>
        /// Gets or sets the width.
        /// </summary>
        public double Width
        {
            get { return width; }
            set { width = value; RaisePropertyChanged(); }
        }

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
