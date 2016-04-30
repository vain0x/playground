using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Dyxi.Muse.Model
{
    public static class MediaCache
    {
        public static string CachePath(int mediaId, string ext)
        {
            return Path.Combine(Auth.Instance.cache_dir, mediaId.ToString() + ext);
        }
        
        public static string Fetch(int mediaId)
        {
            var media = Entity.Instance.medias.Find(mediaId);
            var mediaContent = Entity.Instance.media_contents.Find(mediaId);
            var path = CachePath(mediaId, media.extension);
            if (!File.Exists(path))
            {
                File.WriteAllBytes(path, mediaContent.content);
            }
            return path;
        }
    }
}
