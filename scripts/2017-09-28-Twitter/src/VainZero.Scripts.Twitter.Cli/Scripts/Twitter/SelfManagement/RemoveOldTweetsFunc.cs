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

            var firstDate = new DateTime(2013, 7, 1);
            var endDate = new DateTime(2014, 1, 1);

            //var firstDate = new DateTime(2013, 1, 1);
            //var endDate = new DateTime(2013, 7, 1);

            var csv = new Chilkat.Csv();
            csv.HasColumnNames = true;
            csv.LoadFile(Path);

            for (var i = 0; i < csv.NumRows; i++)
            {
                var id = long.Parse(csv.GetCellByName(i, "tweet_id"));
                var timestamp = DateTime.Parse(csv.GetCellByName(i, "timestamp"));

                if (firstDate <= timestamp && timestamp < endDate)
                {
                    yield return id;
                }
            }
        }

        static async Task<X[]> Collect<X>(IEnumerable<Task<X>> tasks)
        {
            var xs = new List<X>();
            foreach (var task in tasks)
            {
                var x = await task;
                xs.Add(x);
            }
            return xs.ToArray();
        }

        public async Task RemoveOldTweetsAsync()
        {
            RateLimit.RateLimitTrackerMode = RateLimitTrackerMode.TrackAndAwait;



            var tweetIds = TweetIds().ToArray();
            Console.WriteLine("Count = " + tweetIds.Length);


            var tweets =
                Tweet.GetTweets(tweetIds)
                .Where(tweet => tweet != null)
                .ToArray();
            Console.WriteLine("Active Count = " + tweets.Length);


            Console.WriteLine("OK? (Y/n)");
            if (Console.ReadLine() == "Y")
            {
                foreach (var tweet in tweets)
                {
                    try
                    {
                        if (tweet.IsTweetDestroyed)
                        {
                            Debug.WriteLine("DESTROYED: " + tweet.Url);
                            continue;
                        }

                        if (tweet.Media != null && tweet.Media.Count > 0)
                        {
                            if (!tweet.IsRetweet)
                            {
                                Debug.WriteLine("MEDIA: " + tweet.Url);
                                continue;
                            }
                        }

                        if (tweet.IsRetweet)
                        {
                            tweet.UnRetweet();
                        }
                        else
                        {
                            tweet.Destroy();
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
