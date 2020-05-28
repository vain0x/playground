using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Threading.Tasks
{
    static class BooleanTaskModule
    {
        static Task<bool> TrueTask { get; } =
            Task.FromResult(true);

        static Task<bool> FalseTask { get; } =
            Task.FromResult(false);

        public static Task<bool> FromResult(bool value)
        {
            return value ? TrueTask : FalseTask;
        }
    }
}
