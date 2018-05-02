using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading.Tasks;
using Tweetinvi;
using Tweetinvi.Models;

namespace VainZero.Scripts.Twitter.Authentication
{
    public sealed class Authenticator
    {
        void OpenUrl(string url)
        {
            Process.Start(@"C:\Program Files (x86)\Google\Chrome\Application\chrome.exe", "-- " + url);
        }

        public async Task<ITwitterCredentials> AuthenticateAsync()
        {
            Console.WriteLine("Consumer Key?");
            var consumerKey = Console.ReadLine();
            Console.WriteLine("Consumer secret?");
            var consumerSecret = Console.ReadLine();

            var appCred = new TwitterCredentials(consumerKey, consumerSecret);
            var authContext = AuthFlow.InitAuthentication(appCred);

            OpenUrl(authContext.AuthorizationURL);

            Console.WriteLine("Pin code?");
            var pinCode = await Console.In.ReadLineAsync();
            var userCred = AuthFlow.CreateCredentialsFromVerifierCode(pinCode, authContext);

            return userCred;
        }

        public async Task<ITwitterCredentials> AuthenticateWithCachingAsync()
        {
            var configRepo = new ConfigRepository();

            var config = configRepo.LoadNullable();
            if (config != null)
            {
                var t = config.TwitterAccessToken;
                var userCred =
                    new TwitterCredentials(
                        t.ConsumerKey,
                        t.ConsumerSecret,
                        t.AccessTokenKey,
                        t.AccessTokenSecret
                    );
                return userCred;
            }
            else
            {
                var userCred = await AuthenticateAsync();

                var twitterAccessToken =
                    new TwitterAccessToken(
                        userCred.ConsumerKey,
                        userCred.ConsumerSecret,
                        userCred.AccessToken,
                        userCred.AccessTokenSecret
                    );
                var newConfig = new Config(twitterAccessToken);

                configRepo.Save(newConfig);
                return userCred;
            }
        }

        public async Task<ITwitterCredentials> LoginAsync()
        {
            var userCred = await AuthenticateWithCachingAsync();
            Auth.SetCredentials(userCred);
            return userCred;
        }
    }
}
