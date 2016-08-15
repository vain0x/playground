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
        Random Random { get; } = new Random();

        GitHubClient GitHubClient { get; } =
            new GitHubClient(new ProductHeaderValue("rx-learning"));

        ReactiveProperty<string> Requests { get; } =
            new ReactiveProperty<string>("https://api.github.com/users");

        public IObservable<IObservable<User>> Users { get; }

        Task<SearchUsersResult> RequestNext()
        {
            var request = new SearchUsersRequest("w") { Page = Random.Next(0, 10) };
            return GitHubClient.Search.SearchUsers(request);
        }

        public void Refresh()
        {
            Requests.ForceNotify();
        }

        public UserRecommendation()
        {
            Users =
                Requests
                .Select(url =>
                    Observable.FromAsync(() => RequestNext())
                    .SelectMany(result => result.Items)
                );
        }
    }
}
