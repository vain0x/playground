using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Zelga.Core;

namespace Zelga.Wpf
{
    public class MainWindowViewModel
    {
        static readonly User vain = new User("vain", "vain@example.com");
        static readonly User uedai = new User("uedai", "uedai@example.com");
        public User[] Users = new User[] { vain, uedai };

        public Todo[] Todos =
            new Todo[]
            {
                Todo.Create("hello", vain),
                Todo.Create("world", uedai),
            };
    }
}
