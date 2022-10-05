using System.Collections.ObjectModel;
using System.Linq;

namespace AppDesktop
{
    internal sealed class RecordsListPageVm : BindableBase
    {
        public ObservableCollection<RecordListItemVm> Items { get; }

        public EventCommand<object?> BackCommand { get; }
        public EventCommand<object?> CreateCommand { get; }

        public RecordsListPageVm(RecordListItem[] items)
        {
            Items = new(items.Select(item => new RecordListItemVm(item.RecordId, item.Subject)));

            BackCommand = EventCommand.Create<object?>(this);
            CreateCommand = EventCommand.Create<object?>(this);
        }
    }

    internal record RecordData(int RecordId, string Subject, string Contents);

    internal record RecordListItem(int RecordId, string Subject);

    internal sealed class RecordListItemVm : BindableBase
    {
        public int RecordId { get; }
        public string Subject { get; }

        public RecordListItemVm(int recordId, string subject)
        {
            RecordId = recordId;
            Subject = subject;
        }
    }
}
