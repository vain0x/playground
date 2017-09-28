using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Threading.Tasks;
using Tweetinvi;
using Tweetinvi.Models;

namespace VainZero.Scripts.Twitter.Authentication
{
    public sealed class Authenticator
    {
        const string ConsumerKey = "edYfoxeCY45CPJYRPvaGdXG5p";
        const string ConsumerSecret = "g0XRRhQRSA5preS5dPHm1PEaCPwD7SwwuM4ydk32S60lRgwdwY";

        void OpenUrl(string url)
        {
            Process.Start(@"C:\Program Files (x86)\Google\Chrome\Application\chrome.exe", "-- " + url);
        }

        public async Task<ITwitterCredentials> LoginAsync()
        {
            var appCred = new TwitterCredentials(ConsumerKey, ConsumerSecret);
            var authContext = AuthFlow.InitAuthentication(appCred);

            OpenUrl(authContext.AuthorizationURL);

            var pinCode = await Console.In.ReadLineAsync();
            var userCred = AuthFlow.CreateCredentialsFromVerifierCode(pinCode, authContext);
            Auth.SetCredentials(userCred);

            Debug.WriteLine($"var accessToken = \"{userCred.AccessToken}\";");
            Debug.WriteLine($"var accessTokenSecret = \"{userCred.AccessTokenSecret}\";");

            return userCred;
        }

        public ITwitterCredentials LoginAsVain0x()
        {
            var accessToken = "122279728-5N7sWcWZXzAGANMKaSm9iaHkBdKBW4tpHHqUBLbz";
            var accessTokenSecret = "onUWpMXgiXJsY1DWI4zVvJ9gtCACMF0wRgMQZpoyxo9ks";
            return new TwitterCredentials(ConsumerKey, ConsumerSecret, accessToken, accessTokenSecret);
        }
    }
}
