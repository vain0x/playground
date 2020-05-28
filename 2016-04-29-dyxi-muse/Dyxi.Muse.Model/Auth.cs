using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Dyxi.Muse.Model
{
    public static class Auth
    {
        public static user Instance = Login();

        public static user Login()
        {
            var name = "ue_dai";
            var password = "up";

            var user =
                Entity.Instance.users
                .Where(u => u.name == name && u.password == password)
                .FirstOrDefault();
            if (user == null) throw new Exception("Login failed.");
            return user;
        }
    }
}
