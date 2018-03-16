using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Dyxi.Muse.Model
{
    public static class Import
    {
        public static media ImportMediaFile(string path)
        {
            var content = File.ReadAllBytes(path);
            var file = TagLib.File.Create(path);
            var tag = file.Tag;
            var ext = Path.GetExtension(path);

            // TODO: ファイル種別 (音楽か動画か静画か) を判別
            return Entity.AddAudioMedia(tag.Title, tag, ext, file.Length, content);
        }
    }
}
