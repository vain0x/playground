using System.Windows.Controls;
using System.Windows.Data;

namespace AppDesktop
{
    public partial class RecordsListPage : UserControl
    {
        public CollectionViewSource ItemsViewSource { get; } = new();
        private RecordsListPageVm Vm => (RecordsListPageVm)DataContext;

        public RecordsListPage()
        {
            InitializeComponent();

            Loaded += (_, _) =>
            {
                ItemsViewSource.Source = Vm.Items;
                ItemsViewSource.Filter += (_, e) =>
                {
                    var item = (RecordListItemVm)e.Item;
                    e.Accepted = Vm.ApplyFilter(item);
                };
                Vm.FilterChanged += () =>
                {
                    ItemsViewSource.View.Refresh();
                };
            };
        }
    }
}
