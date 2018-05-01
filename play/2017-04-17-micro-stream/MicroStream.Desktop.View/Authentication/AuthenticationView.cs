using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using Microsoft.FSharp.Control;
using Microsoft.FSharp.Core;
using Reactive.Bindings;

namespace MicroStream.Authentication
{
    #region SynchronizationContext.GetAwaiter
    public struct SynchronizationContextAwaiter
        : INotifyCompletion
    {
        readonly SynchronizationContext context;

        public bool IsCompleted =>
            context == null || SynchronizationContext.Current == context;

        public void GetResult()
        {
        }

        public void OnCompleted(Action continuation)
        {
            if (context == null)
            {
                continuation();
            }
            else
            {
                context.Post(_ => continuation(), default(object));
            }
        }

        internal SynchronizationContextAwaiter(SynchronizationContext context)
        {
            this.context = context;
        }
    }

    public static class SynchronizationContextExtension
    {
        public static SynchronizationContextAwaiter GetAwaiter(this SynchronizationContext @this)
        {
            return new SynchronizationContextAwaiter(@this);
        }
    }
    #endregion

    public sealed class AuthenticationView
        : CSharpAuthenticator
    {
        Window Owner { get; }

        SynchronizationContext SyncContext { get; } =
            SynchronizationContext.Current;

        public ReactiveProperty<string> Code { get; } =
            new ReactiveProperty<string>("");

        public ReactiveCommand<Unit> AuthenticateComand { get; } =
            new ReactiveCommand<Unit>();

        AuthenticationWindow CreateWindow()
        {
            Code.Value = "";
            return
                new AuthenticationWindow()
                {
                    DataContext = this,
                    Owner = Owner,
                };
        }

        public override async Task<Tuple<bool, string>> TryAuthenticateAsync(Uri uri)
        {
            Process.Start(uri.ToString());
            await SyncContext;
            var window = CreateWindow();
            using (AuthenticateComand.Subscribe(_ => window.Close()))
            {
                window.ShowDialog();
                var code = Code.Value;
                var hasValue = !string.IsNullOrWhiteSpace(code);
                return Tuple.Create(hasValue, code);
            }
        }

        public AuthenticationView(Window owner)
        {
            Owner = owner;
        }
    }
}
