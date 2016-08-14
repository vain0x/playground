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
    public class UserViewModel
    {
        public User User { get; }
        public string FullName => string.Format("{0} @{1}", User.Name, User.Login);
        public string Bio => User.Bio;

        public UserViewModel(User user)
        {
            User = user;
        }
    }

    public class UserRecommendationViewModel
    {
        UserRecommendation Model { get; } =
            new UserRecommendation();

        public ReactiveCollection<UserViewModel> Users { get; } =
            new ReactiveCollection<UserViewModel>();

        public UserRecommendationViewModel()
        {
            Model.FetchUser()
                .Take(3)
                .Subscribe(user =>
                    Users.AddOnScheduler(new UserViewModel(user)));
        }
    }
}
