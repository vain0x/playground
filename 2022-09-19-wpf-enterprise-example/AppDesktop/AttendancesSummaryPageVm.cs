using System;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Globalization;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;

namespace AppDesktop
{
    internal sealed class AttendancesSummaryPageVm : BindableBase, IDisposable
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

                DismissError();
                AttendanceSummaryDataRequest newRequest = new(value, OnGotData);
                newRequest.OnError += OnError;
                FetchEffect.Invoke(newRequest);
            }
        }

        public bool IsGridDisabled => FetchEffect.IsBusy || ErrorPopupText != null;

        public ObservableCollection<AttendanceTableRowVm> Rows { get; } = new();

        public EventCommand<object?> BackCommand { get; }

        public AttendanceSummaryDataFetchEffect FetchEffect { get; } = new();

        private string? errorPopupText;
        public string? ErrorPopupText
        {
            get => errorPopupText;
            set { errorPopupText = value; RaisePropertyChanged(); RaisePropertyChanged(nameof(IsGridDisabled)); RaisePropertyChanged(nameof(IsErrorPopupShown)); }
        }

        public bool IsErrorPopupShown => ErrorPopupText != null;

        public Command<object?> DismissErrorPopupCommand { get; }

        public AttendancesSummaryPageVm(AttendanceSummaryData data)
        {
            selectedMonth = data.Month;
            monthInput = data.Month.ToString("yyyy/M");

            BackCommand = EventCommand.Create<object?>(this);

            ErrorPopupText = "エラーが発生しました";
            DismissErrorPopupCommand = Command.Create<object?>(_ => DismissError());

            FetchEffect.PropertyChanged += (_, e) =>
            {
                if (e.PropertyName == nameof(AttendanceSummaryDataFetchEffect.IsBusy))
                {
                    RaisePropertyChanged(nameof(IsGridDisabled));
                }
            };

            OnGotData(data);
        }

        public void Dispose()
        {
            if (isDisposed) throw new InvalidOperationException();
            isDisposed = true;

            FetchEffect.Cancel();
        }

        bool isDisposed;

        private void OnGotData(AttendanceSummaryData data)
        {
            Debug.WriteLine($"Got data {data.Month:yyyy-MM}");
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

        private void OnError()
        {
            Rows.Clear();
            ErrorPopupText = "データの取得に失敗しました";
        }

        private void DismissError()
        {
            ErrorPopupText = null;
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
        public Action? OnError { get; set; }
        public Action? OnFinally { get; set; }
        public CancellationTokenSource Cts { get; } = new();

        public AttendanceSummaryDataRequest(DateOnly month, Action<AttendanceSummaryData> onSuccess)
        {
            Month = month;
            OnSuccess = onSuccess;
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

    // データを取得する操作の発生源
    // 後続のリクエストが来たら実行中のリクエストは破棄する
    internal sealed class AttendanceSummaryDataFetchEffect : BindableBase
    {
        // 実行中のリクエスト
        //
        // 起動していないか終了したらnull
        // キャンセルされても終了するまではnullにならない
        private AttendanceSummaryDataRequest? current;

        // 次のリクエスト
        private AttendanceSummaryDataRequest? pending;

        // 新しいリクエストが発生したとき
        public event EventHandler<AttendanceSummaryDataRequest>? Invoked;

        private bool isBusy;
        public bool IsBusy
        {
            get => isBusy;
            set { isBusy = value; RaisePropertyChanged(); }
        }

        private long lastId;

        public async void Invoke(AttendanceSummaryDataRequest newRequest)
        {
            Debug.Assert(Application.Current.Dispatcher.Thread == Thread.CurrentThread);

            // Debounce (連続して起こるイベントのうち最後の1個だけ処理する)
            lastId++;
            var localId = lastId;
            await Task.Delay(300, newRequest.Cts.Token);

            if (localId == lastId)
            {
                InvokeCore(newRequest);
            }
        }

        private void InvokeCore(AttendanceSummaryDataRequest newRequest)
        {
            Debug.Assert(Application.Current.Dispatcher.Thread == Thread.CurrentThread);

            if (current != null)
            {
                pending = newRequest;
                Cancel();
                return;
            }

            current = newRequest;
            IsBusy = true;
            newRequest.OnFinally += () =>
            {
                current = null;

                if (pending != null)
                {
                    var request = pending;
                    pending = null;
                    Invoke(request);
                    return;
                }

                IsBusy = false;
            };
            Invoked?.Invoke(this, newRequest);
        }

        public void Cancel()
        {
            Debug.Assert(Application.Current.Dispatcher.Thread == Thread.CurrentThread);

            if (current != null)
            {
                Debug.WriteLine("Cancel requested");
                current.Cts.Cancel();
            }
        }
    }
}
