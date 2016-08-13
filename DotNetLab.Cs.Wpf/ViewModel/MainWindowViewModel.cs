using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Media;
using Reactive.Bindings;

namespace DotNetLab.Cs.Wpf.ViewModel
{
    public class MainWindowViewModel
    {
        public ReactiveProperty<string> InputText { get; } =
            new ReactiveProperty<string>(string.Empty);

        public ReactiveProperty<string> Text { get; }

        public ReactiveProperty<Brush> Foreground { get; } =
            new ReactiveProperty<Brush>(Brushes.Black);

        public ReactiveCollection<string> Items { get; } =
            new ReactiveCollection<string>();

        public ReactiveCommand SaveCommand { get; }

        public MainWindowViewModel()
        {
            Text =
                InputText
                .Delay(TimeSpan.FromSeconds(1))
                .Select(s => "Your input: " + s)
                .ToReactiveProperty();

            Foreground =
                InputText
                .Select(s => (Brush)Brushes.Red)
                .Merge(
                    InputText
                    .Throttle(TimeSpan.FromSeconds(2))
                    .Select(s => Brushes.Black)
                    )
                .ToReactiveProperty();

            SaveCommand =
                InputText
                .Select(s => !string.IsNullOrEmpty(s))
                .ToReactiveCommand();
            SaveCommand
                .Subscribe(
                    parameter =>
                    {
                        Items.Insert(0, InputText.Value);
                        InputText.Value = string.Empty;
                    });
        }
    }
}
