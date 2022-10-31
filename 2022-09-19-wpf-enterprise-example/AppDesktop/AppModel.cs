using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace AppDesktop
{
    // 順番と依存:
    //      職員データ
    //      認証 (-> 職員データ)
    //      出退勤 (-> 職員データ)
    //      職員の検索・読み書き (-> 職員データ)
    //      日誌

    /// <summary>
    /// モデル層の実装を詰め込んだもの
    /// </summary>
    internal sealed class AppModel
    {
        // ---------------------------------------
        // 職員データ
        // ---------------------------------------

        private readonly List<EmployeeListItem> employees =
            "Alice,Bob,Charlotte,Don,Eve"
                .Split(",")
                .Select((name, index) => new EmployeeListItem(1 + index, name))
                .ToList();

        private int lastEmployeeId = 5;

        // ---------------------------------------
        // 認証
        // ---------------------------------------

        public async Task<LoginUserInfo?> VerifyLoginAsync(LoginRequest request, CancellationToken ct)
        {
            // note: 実際にはデータベースにアクセスする
            await Task.Delay(request.Password.Length * 501 / 8, ct);

            if (request.Password != "password")
            {
                var name = string.Concat(request.LoginId[..1].ToUpper(), request.LoginId[1..]);
                return new LoginUserInfo(1, name);
            }
            else
            {
                return null;
            }
        }

        public async Task SavePasswordAsync(PasswordChangeRequest request, CancellationToken ct)
        {
            // note: データベースに保存する
            await Task.Delay(100, ct);

            if (request.CurrentPassword == "password")
            {
                throw new Exception("パスワードが違います");
            }

            Debug.WriteLine($"パスワードを変更しました: '{request.NewPassword}'");
        }

        // ---------------------------------------
        // 出退勤
        // ---------------------------------------

        private bool isAttended;
        private DateTime? attendanceTime;

        // TODO: 日付とログインユーザーのIDを受け取る、非同期で行う
        public AttendanceStatus GetAttendanceStatus() =>
            new(isAttended, attendanceTime);

        public (bool changed, AttendanceStatus newStatus) SetAttend()
        {
            if (isAttended)
            {
                return (false, GetAttendanceStatus());
            }

            isAttended = true;
            attendanceTime = DateTime.Now;
            return (true, GetAttendanceStatus());
        }

        public (bool changed, AttendanceStatus newStatus) SetLeave()
        {
            if (!isAttended)
            {
                return (false, GetAttendanceStatus());
            }

            isAttended = false;
            return (true, GetAttendanceStatus());
        }

        public AttendanceSummaryData GenerateDummyAttendanceSummary()
        {
            var month = DateOnly.FromDateTime(DateTime.Now);
            month = month.AddDays(1 - month.Day);

            return new AttendanceSummaryData(month, new AttendanceSummaryEntry[]
            {
                new(month, month.ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(9.5))), null),
                new(month.AddDays(1), month.AddDays(1).ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(9.5))), month.AddDays(1).ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(17.5)))),
            });
        }

        public async Task<AttendanceSummaryData> FetchAttendancesSummaryAsync(AttendanceSummaryFetchingRequest request, CancellationToken ct)
        {
            Debug.WriteLine($"Fetch attendances {request.Month:yyyy-MM}");
            var month = request.Month;

            await Task.Delay(200 * month.Month, ct);

            if (month.Month == 11)
            {
                // サーバー側のエラーをシミュレーションする
                throw new Exception("11月のデータを取得できません");
            }

            return new AttendanceSummaryData(month, new AttendanceSummaryEntry[]
            {
                new(month, month.ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(9.5))), null),
                new(month.AddDays(1),  month.AddDays(1).ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(9.5))), month.AddDays(1).ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(17.5)))),
            });
        }

        // ---------------------------------------
        // 職員の検索・読み書き
        // ---------------------------------------

        public EmployeeListItem[] GetEmployeeList() => employees.ToArray();

        public int CreateEmployee(CreateEmployeeRequest request)
        {
            lastEmployeeId++;
            var id = lastEmployeeId;
            var employee = new EmployeeListItem(id, request.EmployeeName);
            employees.Add(employee);
            return id;
        }

        public void DeleteEmployees(int[] employeeIds)
        {
            employees.RemoveAll(e => employeeIds.Contains(e.EmployeeId));
        }

        // ---------------------------------------
        // 日誌
        // ---------------------------------------

        private readonly List<RecordData> records =
            new()
            {
                new(1, "Hello!", "Hello, this is the first record."),
                new(2, "Second", "This second record\nis\nmulti-lined."),
            };

        private int lastRecordId = 2;

        public RecordListItem[] GetRecordList() =>
            records
                .Select(record => new RecordListItem(record.RecordId, record.Subject))
                .ToArray();

        public RecordData? FindRecordById(int recordId) =>
            records.Find(item => item.RecordId == recordId);

        public async Task CreateRecordAsync(RecordsAddRequest request, CancellationToken ct)
        {
            await Task.Delay(request.Contents.Length * 10, ct);

            var id = ++lastRecordId;
            records.Add(new RecordData(id, request.Subject, request.Contents));
        }

        public async Task UpdateRecordAsync(int recordId, RecordsAddRequest request, CancellationToken ct)
        {
            await Task.Delay(request.Contents.Length * 10, ct);

            var index = records.FindIndex(r => r.RecordId == recordId);
            if (index < 0)
                throw new Exception("record missing?");

            records[index] = new RecordData(recordId, request.Subject, request.Contents);
        }
    }

    // 公開APIのデータ型:

    internal sealed record EmployeeListItem(int EmployeeId, string EmployeeName);

    internal sealed record LoginUserInfo(int EmployeeId, string EmployeeName);
    internal sealed record PasswordChangeRequest(string CurrentPassword, string NewPassword);

    internal sealed record AttendanceStatus(bool IsAttended, DateTime? AttendanceTime);

    internal sealed record AttendanceSummaryEntry(DateOnly Date, DateTime? AttendedAt, DateTime? LeftAt);

    internal sealed record AttendanceSummaryData(DateOnly Month, AttendanceSummaryEntry[] Entries);

    internal sealed record AttendanceSummaryFetchingRequest(DateOnly Month);
}
