using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Structures
{
    public class HashMapTests
    {
        [Fact]
        public void RandomCheck()
        {
            new MapChecker().RandomCheck();
        }
    }

    public sealed class MapChecker
    {
        static KeyValuePair<K, V>[] ToSortedArray<K, V>(IEnumerable<KeyValuePair<K, V>> source)
        {
            var array = source.ToArray();
            Array.Sort(array, (l, r) => Comparer<K>.Default.Compare(l.Key, r.Key));
            return array;
        }

        public void RandomCheck()
        {
            var random = new Random();
            var map = HashMap.Empty<long, int>();
            var dict = new Dictionary<long, int>();
            for (var i = 0; i < 100000; i++)
            {
                var t = random.Next(0, 10);
                var k = (long)random.Next() << 20;

                if (t == 0)
                {
                    bool found;
                    int mapValue, dictValue;

                    var newMap = map.TryRemove(k, out found, out mapValue);
                    if (!found) mapValue = -1;
                    if (!dict.TryGetValue(k, out dictValue)) dictValue = -1;

                    if (mapValue != dictValue) throw new Exception(string.Format("find({0}) map: {1}, dict: {2}", k, mapValue, dictValue));

                    map = newMap;
                    dict.Remove(k);
                }
                else
                {
                    var v = random.Next();

                    map = map.SetOrAdd(k, v);

                    if (dict.ContainsKey(k))
                    {
                        dict[k] = v;
                    }
                    else
                    {
                        dict.Add(k, v);
                    }
                }
            }

            if (map.Count != dict.Count)
            {
                throw new Exception(string.Format("map count = {0}, dict count = {1}", map.Count, dict.Count));
            }

            var sortedMap = ToSortedArray(map);
            var sortedDict = ToSortedArray(dict);
            if (!sortedMap.SequenceEqual(sortedDict))
            {
                throw new Exception(string.Join("\n", sortedMap.Zip(sortedDict, (l, r) =>
                    string.Format("map[{0}] = {1}, dict[{2}] = {3}", l.Key, l.Value, r.Key, r.Value))));
            }
        }
    }
}
