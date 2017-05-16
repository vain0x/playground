using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Reactive.Bindings;

namespace VainZero.Dancehall.Reactive.Bindings
{
    public static class ReativePropertyExtension
    {
        public static ReactiveProperty<X> MakeReactiveProperty<X>(this X @this)
        {
            return new ReactiveProperty<X>(@this);
        }
    }
}
