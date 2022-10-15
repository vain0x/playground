using System;
using System.Collections.ObjectModel;
using System.Linq;

namespace AppDesktop
{
    internal sealed class RecordsListPageVm : BindableBase
    {
        private string filterInput = "";
        public string FilterInput
        {
            get => filterInput;
            set { filterInput = value; RaisePropertyChanged(); FilterChanged?.Invoke(); }
        }

        public ObservableCollection<RecordListItemVm> Items { get; }

        public EventCommand<object?> BackCommand { get; }
        public EventCommand<object?> CreateCommand { get; }
        public EventCommand<int?> EditCommand { get; }

        public event Action? FilterChanged;

        public RecordsListPageVm(RecordListItem[] items)
        {
            Items = new(items.Select(item => new RecordListItemVm(item.RecordId, item.Subject)));

            BackCommand = EventCommand.Create<object?>(this);
            CreateCommand = EventCommand.Create<object?>(this);
            EditCommand = EventCommand.Create<int?>(this);
        }

        public bool ApplyFilter(RecordListItemVm item) =>
            item.Subject.Contains(FilterInput);
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
