using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace VainZero
{
    public sealed class TwitterAccessToken
    {
        public TwitterAccessToken(string consumerKey, string consumerSecret, string accessTokenKey, string accessTokenSecret)
        {
            ConsumerKey = consumerKey;
            ConsumerSecret = consumerSecret;
            AccessTokenKey = accessTokenKey;
            AccessTokenSecret = accessTokenSecret;
        }

        public string ConsumerKey { get; }
        public string ConsumerSecret { get; }
        public string AccessTokenKey { get; }
        public string AccessTokenSecret { get; }
    }

    public sealed class Config
    {
        public TwitterAccessToken TwitterAccessToken { get; }

        public Config(TwitterAccessToken twitterAccessToken)
        {
            TwitterAccessToken = twitterAccessToken;
        }
    }

    public sealed class ConfigRepository
    {
        const string FileName = @"env.json";

        public void Save(Config config)
        {
            var configData = Utf8Json.JsonSerializer.Serialize(config);
            var configJson = Utf8Json.JsonSerializer.PrettyPrint(configData);
            File.WriteAllText(FileName, configJson);
        }

        public Config LoadNullable()
        {
            if (!File.Exists(FileName)) return null;

            try
            {
                var json = File.ReadAllText(FileName);
                var config = Utf8Json.JsonSerializer.Deserialize<Config>(json);
                return config;
            }
            catch (FileNotFoundException)
            {
                return null;
            }
        }
    }
}
