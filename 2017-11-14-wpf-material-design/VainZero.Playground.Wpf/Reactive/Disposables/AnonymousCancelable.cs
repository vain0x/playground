using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Disposables;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace DotNetKit.Reactive.Disposables
{
    public sealed class AnonymousCancelable
        : ICancelable
    {
        private readonly Func<bool> _isDisposed;
        private Action _dispose;

        public bool IsDisposed
        {
            get
            {
                return _isDisposed();
            }
        }

        public void Dispose()
        {
            var d = Interlocked.Exchange(ref _dispose, null);
            if (d != null) d();
        }

        public AnonymousCancelable(Func<bool> isDisposed, Action dispose)
        {
            if (isDisposed == null)
                throw new ArgumentNullException("isDisposed");
            if (dispose == null)
                throw new ArgumentNullException("dispose");
            _isDisposed = isDisposed;
            _dispose = dispose;
        }
    }
}
