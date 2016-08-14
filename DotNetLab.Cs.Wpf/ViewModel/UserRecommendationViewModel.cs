using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Text;
using System.Threading.Tasks;
using DotNetLab.Cs.Wpf.Model;
using Octokit;
using Reactive.Bindings;

namespace DotNetLab.Cs.Wpf.ViewModel
{
    public class UserRecommendationViewModel
    {
        UserRecommendation Model { get; } =
            new UserRecommendation();

        public ReactiveCollection<User> Users { get; } =
            new ReactiveCollection<User>();

        public UserRecommendationViewModel()
        {
            Model.FetchUser()
                .Take(3)
                .Subscribe(user => Users.AddOnScheduler(user));
        }
    }
}
