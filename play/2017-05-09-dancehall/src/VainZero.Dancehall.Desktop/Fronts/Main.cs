using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Dancehall.Fronts
{
    public sealed class Main
    {
        public PlayController PlayController { get; }

        public Main(PlayController playController)
        {
            PlayController = playController;
        }

        public static Main Create()
        {
            return new Main(new PlayController(new DemoPlayable()));
        }
    }
}
