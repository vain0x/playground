using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Text;
using System.Threading.Tasks;
using Octokit;
using Reactive.Bindings;

namespace DotNetLab.Cs.Wpf.Model
{
    public class UserRecommendation
    {
        GitHubClient GitHubClient { get; } =
            new GitHubClient(new ProductHeaderValue("rx-learning"));

        ReactiveProperty<string> Requests { get; } =
            new ReactiveProperty<string>("https://api.github.com/users");

        public IObservable<User> Users { get; }

        Task<SearchUsersResult> RequestNext()
        {
            var request = new SearchUsersRequest("w");
            return GitHubClient.Search.SearchUsers(request);
        }

        public UserRecommendation()
        {
            Users =
                Requests
                .SelectMany(url => Observable.FromAsync(() => RequestNext()))
                .SelectMany(result => result.Items);
        }
    }
}
