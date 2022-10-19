using System;
using System.ComponentModel;
using System.Threading;
using System.Threading.Tasks;

namespace AppDesktop
{
    internal sealed class NavigationFrame
    {
        private INotifyPropertyChanged page;
        public INotifyPropertyChanged Page
        {
            get => page;
            set { page = value; OnPageChanged?.Invoke(); }
        }

        public Action? OnPageChanged { get; init; }

        public NavigationProcess NavigationProcess { get; }

        public NavigationFrame(INotifyPropertyChanged initialPage)
        {
            page = initialPage;
            NavigationProcess = new(NavigateCoreAsync);
        }

        public void RequestNavigate(Func<INotifyPropertyChanged> creator)
        {
            //StartAsync(async ct =>
            //{
            //    await NavigateAsync(() =>
            //    {
            //        try
            //        {
            //            return Task.FromResult(creator());
            //        }
            //        catch (Exception ex)
            //        {
            //            return Task.FromException<INotifyPropertyChanged>(ex);
            //        }
            //    }, ct);
            //});

            NavigationProcess.Cancel();
            var oldPage = page;
            Page = creator();
            (oldPage as IDisposable)?.Dispose();
        }

        // ページ遷移のリクエストを出し、その完了を待機する
        public async Task NavigateAsync(Func<Task<INotifyPropertyChanged>> asyncCreator, CancellationToken ct)
        {
            await NavigationProcess.InvokeAsync(new(asyncCreator), ct);
        }

        // 実際にページ遷移を行う
        private async Task NavigateCoreAsync(NavigationRequest request, CancellationToken ct)
        {
            var oldPage = page;
            var newPage = await request.AsyncCreator();

            ct.ThrowIfCancellationRequested();
            if (page != oldPage) throw new OperationCanceledException();

            Page = newPage;
            (oldPage as IDisposable)?.Dispose();
        }
    }

    // 非同期ページ遷移のリクエスト
    internal sealed record NavigationRequest(Func<Task<INotifyPropertyChanged>> AsyncCreator);

    internal sealed class NavigationProcess
    {
        private readonly SerialProcess<NavigationRequest> inner;

        public NavigationProcess(Func<NavigationRequest, CancellationToken, Task> navigator)
        {
            inner = new(navigator);
        }

        public async Task InvokeAsync(NavigationRequest request, CancellationToken ct)
        {
            await inner.InvokeAsync(request, ct);
        }

        public void Cancel()
        {
            inner.Cancel();
        }
    }
}
