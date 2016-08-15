using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Linq;
using System.Reactive.Linq;
using System.Text;
using System.Threading.Tasks;
using DotNetLab.Cs.Wpf.Utility.Detail;
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

        public ObservableCollection<UserViewModel> Users { get; }

        public ReactiveCommand RefreshCommand { get; } =
            new ReactiveCommand();

        public UserRecommendationViewModel()
        {
            Users =
                new SelectObservableCollection<User, UserViewModel>(
                    Model.RecommendedUsers,
                    user => new UserViewModel(user)
                );

            RefreshCommand.Subscribe(_ => Model.Refresh());
        }
    }
}
