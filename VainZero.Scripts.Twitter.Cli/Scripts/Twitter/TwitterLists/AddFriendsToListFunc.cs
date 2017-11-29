using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Threading.Tasks;
using Tweetinvi;
using Tweetinvi.Models;

namespace VainZero.Scripts.Twitter.TwitterLists
{
    public sealed class AddFriendsToListFunc
    {
        readonly ITwitterCredentials cred;

        public AddFriendsToListFunc(ITwitterCredentials cred)
        {
            this.cred = cred;
        }

        public async Task AddFriendsToList()
        {
            //var userId = "rkgk_mochi_112";
            foreach (var targetName in new[] { "zombie_haruna", "zombie_you" })
            {
                var me = await UserAsync.GetAuthenticatedUser(cred);
                var listName = $"home-{targetName}";
                var twitterList = TwitterList.CreateList(listName, PrivacyMode.Private, "Just for test of my application.");

                var target = await UserAsync.GetUserFromScreenName(targetName);

                if (targetName.Contains("haruna"))
                {
                    var users = await UserAsync.GetFriendIds(target, maxFriendsToRetrieve: 100);

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
    }
}
