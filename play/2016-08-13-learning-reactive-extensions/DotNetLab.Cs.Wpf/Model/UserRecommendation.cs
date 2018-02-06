using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
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

        Task<SearchUsersResult> RequestNext()
        {
            var request =
                new SearchUsersRequest("w")
                {
                    Page = Random.Next(0, 10)
                };
            return GitHubClient.Search.SearchUsers(request);
        }

        public ObservableCollection<User> RecommendedUsers { get; }

        public ReactiveCommand<int> ShowAnotherCommand { get; } =
            new ReactiveCommand<int>();

        public ReactiveCommand RefreshCommand { get; } =
            new ReactiveCommand();

        public UserRecommendation()
        {
            var users =
                Observable.Return("https://api.github.com/users")
                .SelectMany(url =>
                    Observable.FromAsync(() => RequestNext())
                    .Repeat())
                .SelectMany(result => result.Items);

            var userIndexes =
                Enumerable.Range(0, 3)
                .Select(i => new ReactiveProperty<int>(i))
                .ToArray();

            RecommendedUsers =
                new ObservableCollection<User>(userIndexes.Select(i => (User)null));

            var userRecommendations =
                users
                .Zip(userIndexes.Merge(), Tuple.Create);

            userRecommendations
                .ObserveOn(UIDispatcherScheduler.Default)
                .Subscribe(t =>
                {
                    var user = t.Item1;
                    var i = t.Item2;
                    RecommendedUsers[i] = user;
                });

            ShowAnotherCommand.Subscribe(i => userIndexes[i].ForceNotify());

            RefreshCommand.Subscribe(_ =>
            {
                foreach (var refreshCommand in userIndexes)
                {
                    refreshCommand.ForceNotify();
                }
            });

            RefreshCommand.Execute();
        }
    }
}
