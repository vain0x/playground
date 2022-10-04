using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;

namespace AppDesktop
{
    internal sealed class MainWindowVm : BindableBase
    {
        private INotifyPropertyChanged currentPage;
        public INotifyPropertyChanged CurrentPage
        {
            get => currentPage;
            set
            {
                var oldPage = currentPage;

                currentPage = value;
                RaisePropertyChanged();

                (oldPage as IDisposable)?.Dispose();
            }
        }

        private bool isBusy;
        public bool IsBusy
        {
            get => isBusy;
            set { isBusy = value; RaisePropertyChanged(); }
        }

        private LoginInfo? loginInfo;
        public LoginInfo? LoginInfo
        {
            get => loginInfo;
            set { loginInfo = value; RaisePropertyChanged(); }
        }

        private bool isProfileDialogOpen;
        public bool IsProfileDialogOpen
        {
            get => isProfileDialogOpen;
            set { isProfileDialogOpen = value; RaisePropertyChanged(); }
        }

        private UsersProfilePageVm? profileDialogVm;
        public UsersProfilePageVm? ProfileDialog
        {
            get => profileDialogVm;
            set { profileDialogVm = value; RaisePropertyChanged(); }
        }

        public Command<object?> GoProfileCommand { get; }
        public Command<object?> GoPasswordChangeCommand { get; }

        private readonly List<EmployeeListItem> dummyEmployees =
            "Alice,Bob,Charlotte,Don,Eve"
                .Split(",")
                .Select((name, index) => new EmployeeListItem(1 + index, name))
                .ToList();

        private bool isAttended;
        private DateTime? attendanceTime;

        private int lastEmployeeId = 5;
        private int busyLevel;

        private event EventHandler<Exception>? AsyncOperationFailed;

        public MainWindowVm()
        {
            GoProfileCommand = Command.Create<object?>(_ => OpenProfileDialog());
            GoPasswordChangeCommand = Command.Create<object?>(_ => OpenUsersPasswordChangePage());

            var loginPage = new LoginPageVm();
            loginPage.OnLoginRequested += (_, request) => Login(request);
            currentPage = loginPage;

#if DEBUG
            // デバッグ用: 毎回ログインするのは面倒なので自動でログインする
            var loginRequest = new LoginRequest("john", "john_password", () => { }, () => { });
            loginPage.LoginId = loginRequest.LoginId;
            loginPage.Password = loginRequest.Password;
            loginPage.LoginCommand.Execute(loginRequest);
#endif
        }

        private void OpenProfileDialog()
        {
            if (LoginInfo == null) return;

            var dialog = new UsersProfilePageVm(LoginInfo);
            dialog.GoPasswordChangeCommand.Executed += (_, _) =>
            {
                CloseProfileDialog();
                OpenUsersPasswordChangePage();
            };
            dialog.CloseCommand.Executed += (_, _) => CloseProfileDialog();

            ProfileDialog = dialog;
            IsProfileDialogOpen = true;
        }

        private void CloseProfileDialog()
        {
            IsProfileDialogOpen = false;
            ProfileDialog = null;
        }

        private void OpenUsersPasswordChangePage()
        {
            var page = new UsersPasswordChangePageVm();
            page.Requested += (_, request) => SavePassword(request);
            CurrentPage = page;
        }

        private void SavePassword(PasswordChangeRequest request)
        {
            StartAsync(async ct =>
            {
                await Task.Delay(100, ct);

                if (request.CurrentPassword == "password")
                {
                    throw new Exception("パスワードが違います");
                }

                Debug.WriteLine($"パスワードを変更しました: '{request.NewPassword}'");
            });
        }

        private void Login(LoginRequest request)
        {
            StartAsync(async ct =>
            {
                var verified = await VerifyLoginAsync(request, ct);
                if (!verified)
                {
                    request.OnFailed();
                    request.OnFinally();
                    return;
                }

                // ログイン成功時:
                LoginInfo = new LoginInfo()
                {
                    Username = string.Concat(request.LoginId[..1].ToUpper(), request.LoginId[1..]),
                };

                OpenHomePage();
                request.OnFinally();
            });
        }

        // note: モデル層に書く
        private async Task<bool> VerifyLoginAsync(LoginRequest request, CancellationToken ct)
        {
            // note: 実際にはデータベースにアクセスする
            await Task.Delay(request.Password.Length * 501 / 8, ct);

            return request.Password != "password";
        }

        private void OpenHomePage()
        {
            var page = new HomePageVm()
            {
                IsAttended = isAttended,
                AttendanceTime = attendanceTime,
            };
            page.Attended += (_, _) =>
            {
                Attend();
                page.AttendanceTime = attendanceTime;
                page.IsAttended = isAttended;
            };
            page.Left += (_, _) =>
            {
                Leave();
                page.IsAttended = isAttended;
            };
            page.GoEmployeesCommand.Executed += (_, _) => OpenEmployeesListPage();
            page.GoAttendancesCommand.Executed += (_, _) => OpenAttendancesSummaryPage();

            CurrentPage = page;
        }

        private void Attend()
        {
            if (isAttended)
            {
                Debug.WriteLine("すでに出勤しています");
                return;
            }

            isAttended = true;
            attendanceTime = DateTime.Now;
        }

        private void Leave()
        {
            if (!isAttended)
            {
                Debug.WriteLine("出勤していません");
                return;
            }

            isAttended = false;
        }

        private void OpenAttendancesSummaryPage()
        {
            // Generate dummy data.
            var month = DateOnly.FromDateTime(DateTime.Now);
            month = month.AddDays(1 - month.Day);

            var data = new AttendanceSummaryData(month, new AttendanceSummaryEntry[]
            {
                new(month, month.ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(9.5))), null),
                new(month.AddDays(1), month.AddDays(1).ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(9.5))), month.AddDays(1).ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(17.5)))),
            });

            var page = new AttendancesSummaryPageVm(data);
            page.BackCommand.Executed += (_, _) => OpenHomePage();
            page.FetchEffect.Invoked += (_, request) => FetchAttendancesSummary(request);

            CurrentPage = page;
        }

        private void FetchAttendancesSummary(AttendanceSummaryDataRequest request)
        {
            StartAsync(async ct =>
            {
                try
                {
                    Debug.WriteLine($"Fetch attendances {request.Month:yyyy-MM}");
                    var month = request.Month;

                    await Task.Delay(200 * month.Month, ct);

                    if (month.Month == 11)
                    {
                        // サーバー側のエラーをシミュレーションする
                        throw new Exception("11月のデータを取得できません");
                    }

                    var data = new AttendanceSummaryData(month, new AttendanceSummaryEntry[]
                    {
                        new(month, month.ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(9.5))), null),
                        new(month.AddDays(1),  month.AddDays(1).ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(9.5))), month.AddDays(1).ToDateTime(TimeOnly.FromTimeSpan(TimeSpan.FromHours(17.5)))),
                    });

                    request.OnSuccess(data);
                }
                catch
                {
                    request.OnError?.Invoke();
                    throw;
                }
                finally
                {
                    request.OnFinally?.Invoke();
                }
            }, request.Cts);
        }

        private void OpenEmployeesListPage()
        {
            var page = new EmployeesListPageVm(dummyEmployees.ToArray());
            page.OnCreateRequested += (_, request) => CreateEmployee(request);
            page.OnDeleteRequested += (_, ids) => DeleteEmployees(ids);
            page.BackCommand.Executed += (_, _) => OpenHomePage();

            CurrentPage = page;
        }

        private void CreateEmployee(CreateEmployeeRequest request)
        {
            lastEmployeeId++;
            var id = lastEmployeeId;
            var employee = new EmployeeListItem(id, request.EmployeeName);
            dummyEmployees.Add(employee);

            OpenEmployeesListPage();
        }

        private void DeleteEmployees(int[] employeeIds)
        {
            dummyEmployees.RemoveAll(e => employeeIds.Contains(e.EmployeeId));

            // TODO: update employeesList in-place
            OpenEmployeesListPage();
        }

        private async void StartAsync(Func<CancellationToken, Task> funcAsync, CancellationTokenSource? cts = null, [CallerMemberName] string? name = null)
        {
            Debug.Assert(Application.Current.Dispatcher.Thread == Thread.CurrentThread);

            cts ??= new CancellationTokenSource();

            // UIスレッドにメッセージを投げるためのチャネル
            var context = SynchronizationContext.Current!;
            Debug.Assert(context != null);

            // trueなら処理が瞬間的には完了しなかったことを表す
            // ドハティの閾値にもとづき、0.4s以上経過したらtrueになる
            // なお処理中表示のちらつきを防ぐため、trueになってから0.4s以上は待つ必要がある
            var longRunning = false;

            var timeoutTask = Task.Delay(400, cts.Token).ContinueWith(task =>
            {
                if (task.IsCompletedSuccessfully)
                {
                    Debug.WriteLine($"Task {name} running for 0.4s...");
                    context.Post(_ =>
                    {
                        busyLevel++;
                        IsBusy = true;
                        longRunning = true;
                    }, null);
                }
            }).ContinueWith(_ => Task.Delay(400)).Unwrap();

            await funcAsync(cts.Token).ContinueWith(task =>
            {
                if (longRunning)
                {
                    timeoutTask.ContinueWith(_ =>
                    {
                        context.Post(_ =>
                        {
                            busyLevel--;
                            IsBusy = busyLevel >= 1;
                        }, null);
                    });
                }

                if (task.IsCanceled)
                {
                    Debug.WriteLine($"Task {name} cancelled");
                    return;
                }

                cts.Cancel();

                if (task.IsFaulted)
                {
                    var ex = task.Exception!.InnerExceptions.Count == 1
                        ? task.Exception.InnerException
                        : task.Exception;
                    Debug.WriteLine($"Task {name} failed with {ex}");

                    context.Post(_ =>
                    {
                        AsyncOperationFailed?.Invoke(this, ex!);
                    }, null);
                    return;
                }

                Debug.WriteLine($"Task {name} completed");
            });

            // このメソッドから始まった非同期処理がすべて終わってからメソッドを抜けることを確実にするためにawaitする
            await timeoutTask;
        }
    }

    internal sealed class LoginInfo : BindableBase
    {
        private string username = "";
        public string Username
        {
            get => username;
            set { username = value; RaisePropertyChanged(); }
        }
    }
}
