using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Dyxi.Muse.ViewModel
{
    public class TrackRow
    {
        public int    Ix            { get; set; }
        public string Title         { get; set; }
        public string Composers     { get; set; }
        public string Writers       { get; set; }
        public string Performers    { get; set; }
        public string Added         { get; set; }
        public string LastPlayed    { get; set; }
        public int    PlayCount     { get; set; }
        public int    MediaId       { get; set; }
    }

    public class StackItem
    {
        public int MediaId { get; set; }
        public string Name { get; set; }
    }
}
