using System;
using System.Diagnostics;
using System.Threading.Tasks;
using Tweetinvi;
using Tweetinvi.Models;
using VainZero.Scripts.Twitter.Authentication;

namespace VainZero.Scripts.Twitter
{
    sealed class Program
    {
        ITwitterCredentials me;

        public async Task RunAsync()
        {
            me = new Authenticator().LoginAsVain0x();
        }

        static Task Main(string[] args)
        {
            return new Program().RunAsync();
        }
    }
}
