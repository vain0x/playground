using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
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

        IEnumerable<long> TweetIds()
        {
            const string Path = @"D:\repo\vain0-storage\doc\tweets\vain0x\tweets.csv";

            var dueDate = new DateTime(2011, 1, 1);

            return
                File.ReadAllLines(Path)
                .Skip(1)
                .Select(line =>
                {
                    var i = line.IndexOf(',');
                    if (i < 0) return null;
                    return long.TryParse(line.Substring(0, i).Trim('"'), out var id) ? new long?(id) : null;
                })
                .Where(id => id.HasValue)
                .Select(id => id.Value);

        }

        public async Task RemoveOldTweetsAsync()
        {
            RateLimit.RateLimitTrackerMode = RateLimitTrackerMode.TrackAndAwait;

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
                SinceId = 10359214514L,
            };
            var me = await UserAsync.GetAuthenticatedUser(cred);
            var tweets = me.GetUserTimeline(parameter);



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
