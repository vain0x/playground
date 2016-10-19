using System.Collections.Generic;

namespace AsterSql
{
    static class KeyValuePair
    {
        public static  KeyValuePair<K, V> Create<K, V>(K key, V value)
        {
            return new KeyValuePair<K, V>(key, value);
        }
    }
}
