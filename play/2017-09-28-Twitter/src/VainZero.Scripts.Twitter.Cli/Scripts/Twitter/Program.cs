using System;
using System.Diagnostics;
using System.Threading.Tasks;
using Tweetinvi;
using Tweetinvi.Models;
using VainZero.Scripts.Twitter.Authentication;
using VainZero.Scripts.Twitter.SelfManagement;

namespace VainZero.Scripts.Twitter
{
    sealed class Program
    {
        public async Task RunAsync()
        {
            try
            {
                var cred = await new Authenticator().LoginAsync();

                await new RemoveOldTweetsFunc(cred).RemoveOldTweetsAsync();
            }
            catch (Exception ex)
            {
                Debug.WriteLine(ex);
                throw;
            }
        }

        static void Main(string[] args)
        {
            new Program().RunAsync().GetAwaiter().GetResult();
        }
    }
}
