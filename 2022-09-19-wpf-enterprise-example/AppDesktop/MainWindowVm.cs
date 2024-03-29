using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;

namespace AppDesktop
{
    internal sealed class MainWindowVm : BindableBase
    {
        private readonly AppModel model = new();
        private readonly NavigationFrame navigationFrame;

        public INotifyPropertyChanged CurrentPage => navigationFrame.Page;

        private bool isBusy;
        public bool IsBusy
        {
            get => isBusy;
            set { isBusy = value; RaisePropertyChanged(); }
        }

        private LoginInfoVm? loginInfo;
        public LoginInfoVm? LoginInfo
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

        private int busyLevel;
        private event EventHandler<Exception>? AsyncOperationFailed;

        public MainWindowVm()
        {
            GoProfileCommand = Command.Create<object?>(_ => OpenProfileDialog());
            GoPasswordChangeCommand = Command.Create<object?>(_ => OpenUsersPasswordChangePage());

            navigationFrame = new(CreateLoginPage())
            {
                OnPageChanged = () => RaisePropertyChanged(nameof(CurrentPage)),
            };

#if DEBUG
            // デバッグ用: 毎回ログインするのは面倒なので自動でログインする
            LoginInfo = new() { Username = "John" };
            OpenHomePage();
#endif
        }

        private LoginPageVm CreateLoginPage()
        {
            var page = new LoginPageVm();
            page.OnLoginRequested += (_, request) => Login(request);
            return page;
        }

        private void OpenProfileDialog()
        {
            Debug.Assert(LoginInfo != null);

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
            RequestNavigate(() =>
            {
                var page = new UsersPasswordChangePageVm();
                page.Requested += (_, request) => SavePassword(request);
                page.CancelCommand.Executed += (_, _) => OpenHomePage();
                return page;
            });
        }

        private void SavePassword(PasswordChangeRequest request)
        {
            StartAsync(async ct =>
            {
                await model.SavePasswordAsync(request, ct);
            });
        }

        private void Login(LoginRequest request)
        {
            StartAsync(async ct =>
            {
                try
                {
                    var loginInfo = await model.VerifyLoginAsync(request, ct);
                    if (loginInfo == null)
                    {
                        request.OnFailed();
                        return;
                    }

                    LoginInfo = new LoginInfoVm() { Username = loginInfo.EmployeeName };
                    await OpenHomePageAsync(ct);
                }
                finally
                {
                    request.OnFinally();
                }
            });
        }

        private void OpenHomePage() =>
            StartAsync(ct => OpenHomePageAsync(ct));

        private async Task OpenHomePageAsync(CancellationToken ct)
        {
            Debug.Assert(LoginInfo != null);

            await NavigateAsync(async () =>
            {
                await Task.Delay(100, ct);
                var status = model.GetAttendanceStatus();

                var page = new HomePageVm()
                {
                    IsAttended = status.IsAttended,
                    AttendanceTime = status.AttendanceTime,
                };
                page.Attended += (_, _) =>
                {
                    var (changed, status) = model.SetAttend();
                    if (!changed)
                    {
                        Debug.WriteLine("すでに出勤しています");
                    }

                    page.AttendanceTime = status.AttendanceTime;
                    page.IsAttended = status.IsAttended;
                };
                page.Left += (_, _) =>
                {
                    var (changed, status) = model.SetLeave();
                    if (!changed)
                    {
                        Debug.WriteLine("出勤していません");
                    }

                    page.IsAttended = status.IsAttended;
                };
                page.GoRecordsCommand.Executed += (_, _) => OpenRecordsListPage();
                page.GoEmployeesCommand.Executed += (_, _) => OpenEmployeesListPage();
                page.GoAttendancesCommand.Executed += (_, _) => OpenAttendancesSummaryPage();
                return page;
            }, ct);
        }

        private void OpenAttendancesSummaryPage()
        {
            StartAsync(async ct =>
            {
                await NavigateAsync(async () =>
                {
                    await Task.Delay(100, ct);
                    var data = model.GenerateDummyAttendanceSummary();

                    var page = new AttendancesSummaryPageVm(data);
                    page.BackCommand.Executed += (_, _) => OpenHomePage();
                    page.FetchEffect.Invoked += (_, request) => FetchAttendancesSummary(request);
                    return page;
                }, ct);
            });
        }

        private void FetchAttendancesSummary(AttendanceSummaryDataRequest request)
        {
            StartAsync(async ct =>
            {
                try
                {
                    var data = await model.FetchAttendancesSummaryAsync(request.Payload, ct);
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
            RequestNavigate(() =>
            {
                var page = new EmployeesListPageVm(model.GetEmployeeList());
                page.OnCreateRequested += (_, request) => CreateEmployee(request);
                page.OnDeleteRequested += (_, ids) => DeleteEmployees(ids);
                page.BackCommand.Executed += (_, _) => OpenHomePage();
                return page;
            });
        }

        private void CreateEmployee(CreateEmployeeRequest request)
        {
            model.CreateEmployee(request);
            OpenEmployeesListPage();
        }

        private void DeleteEmployees(int[] employeeIds)
        {
            model.DeleteEmployees(employeeIds);

            // TODO: update employeesList in-place
            OpenEmployeesListPage();
        }

        private void OpenRecordsListPage()
        {
            RequestNavigate(() =>
            {
                var items = model.GetRecordList();

                var page = new RecordsListPageVm(items);
                page.BackCommand.Executed += (_, _) => OpenHomePage();
                page.CreateCommand.Executed += (_, _) => OpenRecordsCreatePage();
                page.EditCommand.Executed += (_, id) => OpenRecordsEditPage(id);
                return page;
            });
        }

        private void OpenRecordsCreatePage()
        {
            RequestNavigate(() =>
            {
                var page = new RecordsCreatePageVm();
                page.CancelCommand.Executed += (_, _) => OpenRecordsListPage();
                page.RequestEffect.Invoked += (_, request) => CreateRecord(request);
                return page;
            });
        }

        private void OpenRecordsEditPage(int? recordId)
        {
            RequestNavigate(() =>
            {
                if (recordId == null)
                    throw new Exception("no id");

                var recordData = model.FindRecordById(recordId.Value);
                if (recordData == null)
                    throw new Exception("record missing");

                var page = new RecordsCreatePageVm
                {
                    Subject = recordData.Subject,
                    Contents = recordData.Contents
                };
                page.CancelCommand.Executed += (_, _) => OpenRecordsListPage();
                page.RequestEffect.Invoked += (_, request) => UpdateRecord(recordId.Value, request);

                return page;
            });
        }

        private void CreateRecord(RecordsAddRequest request)
        {
            StartAsync(async ct =>
            {
                try
                {
                    await model.CreateRecordAsync(request, ct);
                    OpenRecordsListPage();
                }
                finally
                {
                    request.OnFinally?.Invoke();
                }
            });
        }

        private void UpdateRecord(int recordId, RecordsAddRequest request)
        {
            StartAsync(async ct =>
            {
                try
                {
                    await model.UpdateRecordAsync(recordId, request, ct);
                    OpenRecordsListPage();
                }
                finally
                {
                    request.OnFinally?.Invoke();
                }
            });
        }

        /// <summary>
        /// ページ遷移を要求する
        ///
        /// <para>
        /// 注意: 呼び出しの完了時にページ遷移が完了するわけではない
        /// </para>
        /// </summary>
        /// <param name="creator">
        /// ページを生成する関数
        /// </param>
        private void RequestNavigate(Func<INotifyPropertyChanged> creator)
        {
            navigationFrame.RequestNavigate(creator);
        }

        // asyncCreatorの提供者がCancellationTokenを持っているはずなので、トークンを関数に渡す必要はない
        /// <summary>
        /// 非同期でページ遷移を要求する
        /// </summary>
        /// <param name="asyncCreator">
        /// 非同期でページを生成する関数
        /// </param>
        private async Task NavigateAsync(Func<Task<INotifyPropertyChanged>> asyncCreator, CancellationToken ct)
        {
            await navigationFrame.NavigateAsync(asyncCreator, ct);
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

    internal sealed class LoginInfoVm : BindableBase
    {
        private string username = "";
        public string Username
        {
            get => username;
            set { username = value; RaisePropertyChanged(); }
        }
    }
}
