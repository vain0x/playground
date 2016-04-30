using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Dyxi.Muse.Model
{
    public static class Entity
    {
        public static dyxi_museEntities Instance = new dyxi_museEntities();

        public static people TryFindPeopleByName(string name)
        {
            return Instance.peoples.Where(people => people.name == name).FirstOrDefault();
        }
    }
}
