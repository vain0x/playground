using System;
using System.Threading.Tasks;

namespace VainZero.Scripts.Twitter
{
    sealed class Program
    {
        public Task RunAsync()
        {
            return Task.CompletedTask;
        }

        static Task Main(string[] args)
        {
            return new Program().RunAsync();
        }
    }
}
