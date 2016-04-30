using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Dyxi.Muse.Model
{
    public interface IColl
    {
        string Name { get; }
        IEnumerable<media> Items { get; }
    }

    public class MusicColl : IColl
    {
        private static MusicColl _instance;
        public static MusicColl Instance
        {
            get { return (_instance ?? (_instance = new MusicColl())); }
        }
        
        public string Name
        {
            get { return "Music"; }
        }

        public IEnumerable<media> Items
        {
            get { throw new NotImplementedException(); }
        }
    }
}
