using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using System.Windows.Threading;
using DotNetKit.Functional.Commands;
using DotNetKit.Misc;
using DotNetKit.Windows.Controls;
using Reactive.Bindings;

namespace DotNetKit.Wpf.ToastNotification.Demo
{
    public sealed class MainWindowViewModel
    {
        public ToastNotificationCollection ToastNotificationCollection { get; } =
            new ToastNotificationCollection();

        readonly Random random = new Random();
        readonly DispatcherTimer timer =
            new DispatcherTimer()
            {
                Interval = TimeSpan.FromSeconds(3.0),
            };

        SimpleToastNotificationTheme ThemeFromInt(int i)
        {
            switch (i)
            {
                case 0: return SimpleToastNotificationTheme.Success;
                case 1: return SimpleToastNotificationTheme.Info;
                case 2: return SimpleToastNotificationTheme.Error;
                case 3: return SimpleToastNotificationTheme.Warning;
                default: throw new Exception();
            }
        }

        SimpleToastNotificationTheme RandomTheme()
        {
            return ThemeFromInt(random.Next(0, 4));
        }

        void Add(SimpleToastNotificationTheme theme)
        {
            ToastNotificationCollection.Add(
                new SimpleToastNotification(theme)
                {
                    Title = "Demo",
                    Message = "Hello!"
                });
        }

        public MainWindowViewModel()
        {
            timer.Tick += (sender, e) =>
            {
                Add(RandomTheme());
            };

            timer.Start();

            foreach (var i in Enumerable.Range(0, 4))
            {
                Add(ThemeFromInt(i));
            }
        }
    }
}
