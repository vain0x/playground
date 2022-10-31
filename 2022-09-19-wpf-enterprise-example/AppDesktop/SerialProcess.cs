using System;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;

namespace AppDesktop
{
    /// <summary>
    /// 単一実行のプロセス
    /// </summary>
    /// <para>
    /// プロセスが起動中の状態で次の起動が要求されたら、現行のプロセスをキャンセルする
    /// </para>
    internal sealed class SerialProcess<TRequest>
    {
        private (TRequest request, Task task, CancellationTokenSource ctSource)? current;

        private readonly Func<TRequest, CancellationToken, Task> executeAsync;

        public SerialProcess(Func<TRequest, CancellationToken, Task> executeAsync)
        {
            this.executeAsync = executeAsync;
        }

        public async Task InvokeAsync(TRequest request, CancellationToken ct)
        {
            ct.ThrowIfCancellationRequested();

            if (current != null)
            {
                var (_, currentTask, currentCtSource) = current.Value;
                var waitTask = currentTask.ContinueWith(_ => { }, ct);
                currentCtSource.Cancel();
                await waitTask;
            }

            Debug.Assert(current == null);

            var ctSource = CancellationTokenSource.CreateLinkedTokenSource(ct);

            try
            {
                var task = executeAsync(request, ctSource.Token);
                current = (request, task, ctSource);
                await task;
            }
            finally
            {
                current = null;
            }
        }

        public void Cancel()
        {
            current?.ctSource.Cancel();
        }
    }
}
