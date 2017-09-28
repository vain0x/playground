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
        ITwitterCredentials cred;

        public async Task AddListFromUserFriends()
        {
            //var userId = "rkgk_mochi_112";
            foreach (var targetName in new[] { "zombie_haruna", "zombie_you" })
            {

                var me = User.GetAuthenticatedUser(cred);
                var listName = $"home-{targetName}";
                var twitterList = TwitterList.CreateList(listName, PrivacyMode.Private, "Just for test of my application.");

                var target = User.GetUserFromScreenName(targetName);

                if (targetName.Contains("haruna"))
                {
                    var users = User.GetFriendIds(target, maxFriendsToRetrieve: 100);

                    foreach (var userId in users)
                    {
                        var result = twitterList.AddMember(userId);
                        Debug.WriteLine(result);
                    }
                }
                else
                {
                    continue;
                }
            }
        }

        public async Task RunAsync()
        {
            try
            {
                cred = new Authenticator().LoginAsVain0x();

                await AddListFromUserFriends();
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
