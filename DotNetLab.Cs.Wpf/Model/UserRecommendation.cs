using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Reactive.Threading.Tasks;
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

        IObservable<User> Users { get; set; }
        
        Task<SearchUsersResult> RequestNext()
        {
            var request = new SearchUsersRequest("w") { Page = Random.Next(0, 10) };
            return GitHubClient.Search.SearchUsers(request);
        }

        IObservable<User> PopUsers(int n)
        {
            var users = Users.Take(n);
            Users = Users.Skip(n);
            return users;
        }

        public ReactiveCollection<User> RecommendedUsers { get; } =
            new ReactiveCollection<User>();

        public void Refresh()
        {
            var users = PopUsers(3).ToEnumerable();
            RecommendedUsers.Clear();
            RecommendedUsers.AddRangeOnScheduler(users);
        }

        public void ShowAnother(int i)
        {
            var user = PopUsers(1).Wait();
            RecommendedUsers[i] = user;
        }

        public UserRecommendation()
        {
            Users =
                Requests
                .SelectMany(url => Observable.FromAsync(() => RequestNext()))
                .SelectMany(result => result.Items);

            Refresh();
        }
    }
}
