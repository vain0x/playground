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
        
        private static string Fetch(int mediaId)
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

        public static Task<string> FetchAsync(int mediaId)
        {
            Task<string> task;
            if (!CacheTask.TryGetValue(mediaId, out task))
            {
                task = Task.Run(() => Fetch(mediaId));
                CacheTask.Add(mediaId, task);
            }
            return task;
        }

        public static Dictionary<int, Task<string>> CacheTask =
            new Dictionary<int, Task<string>>();
    }
}
