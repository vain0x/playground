using System;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Globalization;

namespace AppDesktop
{
    internal sealed class AttendancesSummaryPageVm : BindableBase
    {
        private string monthInput = "";
        public string MonthInput
        {
            get => monthInput;
            set
            {
                monthInput = value;
                RaisePropertyChanged();

                if (DateOnly.TryParseExact(value, "yyyy/M", CultureInfo.CurrentCulture, DateTimeStyles.None, out var date))
                {
                    SelectedMonth = date.AddDays(1 - date.Day);
                }
            }
        }

        private DateOnly selectedMonth;
        public DateOnly SelectedMonth
        {
            get => selectedMonth;
            set
            {
                selectedMonth = value;
                RaisePropertyChanged();

                IsRequested = true;
                OnDataRequested?.Invoke(this, new(
                    value,
                    OnGotData,
                    () => { },
                    () => IsRequested = false
                ));
            }
        }

        private bool isRequested;
        public bool IsRequested
        {
            get => isRequested;
            set { isRequested = value; RaisePropertyChanged(); }
        }

        public ObservableCollection<AttendanceTableRowVm> Rows { get; } = new();

        public EventCommand<object?> BackCommand { get; }

        public event EventHandler<AttendanceSummaryDataRequest>? OnDataRequested;

        public AttendancesSummaryPageVm(AttendanceSummaryData data)
        {
            selectedMonth = data.Month;
            monthInput = data.Month.ToString("yyyy/M");

            BackCommand = EventCommand.Create<object?>(this);

            OnGotData(data);
        }

        private void OnGotData(AttendanceSummaryData data)
        {
            Rows.Clear();

            var lastDay = data.Month.AddMonths(1).AddDays(-1).Day;
            var ei = 0;
            for (var d = 1; d <= lastDay; d++)
            {
                if (ei < data.Entries.Length && data.Entries[ei].Date.Day == d)
                {
                    var entry = data.Entries[ei];
                    Rows.Add(new(
                        entry.Date,
                        entry.AttendedAt,
                        entry.LeftAt
                    ));
                    ei++;
                }
                else
                {
                    Rows.Add(new(data.Month.AddDays(d - 1), null, null));
                }
            }
        }
    }

    // model:
    internal sealed class AttendanceSummaryData
    {
        public DateOnly Month { get; }
        public AttendanceSummaryEntry[] Entries { get; }

        public AttendanceSummaryData(DateOnly month, AttendanceSummaryEntry[] entries)
        {
            Month = month;
            Entries = entries;
        }
    }

    internal sealed class AttendanceSummaryEntry
    {
        public DateOnly Date { get; }
        public DateTime? AttendedAt { get; }
        public DateTime? LeftAt { get; }

        public AttendanceSummaryEntry(DateOnly date, DateTime? attendedAt, DateTime? leftAt)
        {
            Date = date;
            AttendedAt = attendedAt;
            LeftAt = leftAt;
        }
    }

    internal sealed class AttendanceSummaryDataRequest
    {
        public DateOnly Month { get; }
        public Action<AttendanceSummaryData> OnSuccess { get; }
        public Action OnError { get; }
        public Action OnFinally { get; }

        public AttendanceSummaryDataRequest(DateOnly month, Action<AttendanceSummaryData> onSuccess, Action onError, Action onFinally)
        {
            Month = month;
            OnSuccess = onSuccess;
            OnError = onError;
            OnFinally = onFinally;
        }
    }

    // other vm:
    internal sealed class AttendanceTableRowVm : BindableBase
    {
        public DateOnly Date { get; }
        public DateTime? AttendedAt { get; }
        public DateTime? LeftAt { get; }

        public AttendanceTableRowVm(DateOnly date, DateTime? attendedAt, DateTime? leftAt)
        {
            Date = date;
            AttendedAt = attendedAt;
            LeftAt = leftAt;
        }
    }
}
