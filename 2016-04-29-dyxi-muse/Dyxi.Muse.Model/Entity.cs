using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TagLib;

namespace Dyxi.Muse.Model
{
    public static class Entity
    {
        public static dyxi_museEntities Instance = new dyxi_museEntities();

        public static void Commit()
        {
            Instance.SaveChanges();
        }

        public static people TryFindPeopleByName(string name)
        {
            return Instance.peoples.Where(people => people.name == name).FirstOrDefault();
        }

        public static people FindOrAddPeople(string name)
        {
            var people = TryFindPeopleByName(name);
            if (people == null)
            {
                people = Instance.peoples.Add(new people() { name = name });
                Commit();
            }
            return people;
        }

        public static void AddComposersToWork(int workId, string[] names)
        {
            foreach (var name in names)
            {
                var people = FindOrAddPeople(name);
                Instance.work_composers.Add(new work_composers
                {
                    work_id = workId,
                    people_id = people.id
                });
            }
        }

        public static work AddAudioWork(string name, string[] composerNames)
        {
            var work = Instance.works.Add(new work { name = name });
            Commit();
            AddComposersToWork(work.id, composerNames);
            return work;
        }

        public static void AddPerformersToMedia(int mediaId, string[] performerNames)
        {
            foreach (var name in performerNames)
            {
                var people = FindOrAddPeople(name);
                Instance.media_performers.Add(new media_performers
                {
                    media_id = mediaId,
                    people_id = people.id,
                    role = null
                });
            }
        }

        public static media AddAudioMedia
            (string workName, Tag tag, string ext, long length, byte[] content)
        {
            var work = AddAudioWork(workName, tag.Composers);
            var media =
                Instance.medias.Add(new media
                {
                    name = tag.Title,
                    length = length,
                    extension = ext,
                    work_id = work.id
                });
            Commit();
            Instance.media_contents.Add(new media_contents()
            {
                media_id = media.id,
                content = content
            });
            Commit();
            return media;
        }
    }
}
