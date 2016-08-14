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
        public ReactiveProperty<string> RequestStream { get; } =
            new ReactiveProperty<string>("https://api.github.com/users");

        public IObservable<User> FetchUser()
        {
            var github = new GitHubClient(new ProductHeaderValue("rx-learning"));
            return
                Observable.FromAsync(() => github.Search.SearchUsers(new SearchUsersRequest("w")))
                .SelectMany(result => result.Items);
        }
    }
}
