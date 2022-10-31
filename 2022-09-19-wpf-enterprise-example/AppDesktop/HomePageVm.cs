using System;
using System.Diagnostics;

namespace AppDesktop
{
    internal sealed class HomePageVm : BindableBase
    {
        private bool isAttended;
        public bool IsAttended
        {
            get => isAttended;
            set { isAttended = value; RaisePropertyChanged(); RaisePropertyChanged(nameof(AttendanceStatus)); AttendCommand.RaiseCanExecuteChanged(); LeaveCommand.RaiseCanExecuteChanged(); }
        }

        private DateTime? attendanceTime;
        public DateTime? AttendanceTime
        {
            get => attendanceTime;
            set { attendanceTime = value; RaisePropertyChanged(); RaisePropertyChanged(nameof(AttendanceStatus)); AttendCommand.RaiseCanExecuteChanged(); LeaveCommand.RaiseCanExecuteChanged(); }
        }

        public string AttendanceStatus
        {
            get
            {
                if (!isAttended && attendanceTime == null) return "";

                if (isAttended)
                {
                    Debug.Assert(attendanceTime != null);
                    return string.Format(@"{0:HH\:mm}出勤", attendanceTime.Value);
                }

                return "本日は退勤済みです";
            }
        }

        public Command<object?> AttendCommand { get; }
        public Command<object?> LeaveCommand { get; }
        public EventCommand<object?> GoRecordsCommand { get; }
        public EventCommand<object?> GoEmployeesCommand { get; }
        public EventCommand<object?> GoAttendancesCommand { get; }

        public event EventHandler? Attended;
        public event EventHandler? Left;

        public HomePageVm()
        {
            AttendCommand = Command.CreateWithCanExecute<object?>(
                _ => !IsAttended && AttendanceTime == null,
                _ => Attended?.Invoke(this, EventArgs.Empty)
            );

            LeaveCommand = Command.CreateWithCanExecute<object?>(
                _ => IsAttended && AttendanceTime != null,
                _ => Left?.Invoke(this, EventArgs.Empty)
            );

            GoRecordsCommand = EventCommand.Create<object?>(this);
            GoEmployeesCommand = EventCommand.Create<object?>(this);
            GoAttendancesCommand = EventCommand.Create<object?>(this);
        }
    }
}
