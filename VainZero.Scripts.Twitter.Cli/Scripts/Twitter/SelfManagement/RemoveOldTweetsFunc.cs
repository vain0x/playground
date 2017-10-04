using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Tweetinvi;
using Tweetinvi.Models;

namespace VainZero.Scripts.Twitter.SelfManagement
{
    public sealed class RemoveOldTweetsFunc
    {
        readonly ITwitterCredentials cred;

        public RemoveOldTweetsFunc(ITwitterCredentials cred)
        {
            this.cred = cred ?? throw new ArgumentNullException(nameof(cred));
        }

        public async Task RemoveOldTweetsAsync()
        {
            RateLimit.RateLimitTrackerMode = RateLimitTrackerMode.TrackAndAwait;

            var dueDate = new DateTime(2011, 1, 1);

            /*
            var maxTweetSearch = Search.SearchTweets(new Tweetinvi.Parameters.SearchTweetsParameters("from:vain0x")
            {
                TweetSearchType = Tweetinvi.Parameters.TweetSearchType.OriginalTweetsOnly,
                Since = dueDate.AddDays(-3),
                Until = dueDate,
            });
            var maxTweetId =
                maxTweetSearch
                .Select(tweet => Tuple.Create(tweet.CreatedAt, tweet.Id))
                .Max()
                .Item2;
            */
            var maxTweetId = 20537533599326208L;

            var parameter = new Tweetinvi.Parameters.UserTimelineParameters()
            {
                MaxId = maxTweetId,
            };
            var me = await UserAsync.GetAuthenticatedUser(cred);
            var tweets = await me.GetUserTimelineAsync(parameter);



            foreach (var tweet in tweets)
            {
                if (tweet.CreatedAt >= dueDate)
                {
                    Debug.Assert(false);
                }

                Debug.WriteLine(tweet.Text.Substring(40));
            }

            if (Console.ReadLine() == "Y")
            {
                foreach (var tweet in tweets)
                {
                    try
                    {
                        if (tweet.CreatedBy != me)
                        {
                            Debug.WriteLine("Not mine: " + tweet.Url);
                            continue;
                        }

                        if (tweet.IsTweetDestroyed)
                        {
                            Debug.WriteLine("DESTROYED: " + tweet.Url);
                            continue;
                        }

                        if (tweet.Media?.Count > 0)
                        {
                            Debug.WriteLine("has-media: " + tweet.Url);
                            continue;
                        }

                        if (tweet.IsRetweet)
                        {
                            tweet.UnRetweet();
                        }
                        else
                        {
                            await tweet.DestroyAsync();
                        }
                    }
                    catch (Exception ex)
                    {
                        Debug.WriteLine(ex);
                        Debug.Assert(false);

                        Console.WriteLine("Stop?");
                        if (Console.ReadLine() == "N") return;
                    }
                }
            }
        }
    }
}
