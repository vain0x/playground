using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Threading;

namespace AppDesktop
{
    internal sealed class MainWindowVm : BindableBase
    {
        private INotifyPropertyChanged currentPage;
        public INotifyPropertyChanged CurrentPage
        {
            get => currentPage;
            set { currentPage = value; RaisePropertyChanged(); }
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

        private readonly List<EmployeeListItem> dummyEmployees =
            "Alice,Bob,Charlotte,Don,Eve"
                .Split(",")
                .Select((name, index) => new EmployeeListItem(1 + index, name))
                .ToList();

        private int lastEmployeeId = 5;
        private int busyLevel;

        private event EventHandler<Exception>? AsyncOperationFailed;

        public MainWindowVm()
        {
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
            var page = new HomePageVm();
            page.GoEmployeesCommand.Executed += (_, _) => OpenEmployeesListPage();

            CurrentPage = page;
        }

        private void OpenEmployeesListPage()
        {
            var page = new EmployeesListPageVm(dummyEmployees.ToArray());
            page.CreateCommand.Executed += (_, _) => OpenEmployeesCreatePage();
            page.OnDeleteRequested += (_, ids) => DeleteEmployees(ids);
            page.BackCommand.Executed += (_, _) => OpenHomePage();

            CurrentPage = page;
        }

        private void OpenEmployeesCreatePage()
        {
            var page = new EmployeesCreatePageVm();
            page.OnCreateRequested += (_, request) => CreateEmployee(request);
            page.CancelCommand.Executed += (_, _) => OpenEmployeesListPage();
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

        private async void StartAsync(Func<CancellationToken, Task> funcAsync, [CallerMemberName] string? name = null)
        {
            using var cts = new CancellationTokenSource();

            // UIスレッドにメッセージを投げるためのチャネル
            var context = SynchronizationContext.Current!;

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
