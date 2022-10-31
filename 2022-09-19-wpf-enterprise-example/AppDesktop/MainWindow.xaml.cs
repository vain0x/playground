using System.Windows;
using System.Windows.Input;

namespace AppDesktop
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            Loaded += (_, _) =>
            {
                var title = "WPF Enterprise Example";

                var vm = (MainWindowVm)DataContext;
                vm.PropertyChanged += (_, e) =>
                {
                    if (e.PropertyName == nameof(MainWindowVm.IsBusy))
                    {
                        // インディケーター代わりにタイトルとカーソルを変更する
                        Title = (vm.IsBusy ? $"[処理中] " : "") + title;
                        Cursor = vm.IsBusy ? Cursors.Wait : Cursors.Arrow;
                    }
                };
            };
        }
    }
}
