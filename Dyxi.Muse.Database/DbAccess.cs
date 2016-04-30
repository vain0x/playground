using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.FSharp.Core;
using Dyxi.Util.FSharp;

namespace Dyxi.Muse.Database
{
    public class DbAccess
    {
        public DbAccess()
        {
            this.Entities = new DyxiMuseEntities();
        }

        public FSharpOption<users> TryFindUserByName(string name)
        {
            var users =
                from u in this.Entities.users
                where u.name == name
                select u;
            return users.TryFirst();
        }

        public users FindOrAddUser(string name)
        {
            var userOpt = this.TryFindUserByName(name);
            if (userOpt.IsSome())
            {
                return userOpt.Value;
            }
            else
            {
                return this.Entities.users.Add(new users { name = name });
            }
        }

        public IEnumerable<medias> AllMedias()
        {
            return
                from m in this.Entities.medias
                select m;
        }

        public DyxiMuseEntities Entities { get; private set; }
    }
}
