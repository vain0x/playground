using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Misc.Disposables
{
    public sealed class AnonymousDisposable
        : IDisposable
    {
        public AnonymousDisposable(Action dispose)
        {
            this.dispose = dispose;
        }

        readonly Action dispose;

        public void Dispose()
        {
            dispose();
        }
    }
}
