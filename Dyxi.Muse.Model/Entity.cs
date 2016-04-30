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

        public static people FindOrAddPeople(string name)
        {
            var people = Instance.peoples.Find();
            if (people == null)
            {
                return Instance.peoples.Add(new people() { name = name });
            }
            else
            {
                return people;
            }
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
    }
}
