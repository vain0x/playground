using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using System.Windows.Media;
using Reactive.Bindings;
using DotNetLab.Cs.Wpf.Model;

namespace DotNetLab.Cs.Wpf.ViewModel
{
    public class MainWindowViewModel
    {
        public UserRecommendationViewModel UserRecommendationViewModel { get; } =
            new UserRecommendationViewModel();
    }
}
