using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Paginating
{
    /// <summary>
    /// リストをあるプロパティーについて昇順または降順でソートする、という指示を表す。
    /// </summary>
    public sealed class PropertySort
    {
        /// <summary>
        /// ソートの基準となるプロパティーを特定するためのオブジェクトを取得する。
        /// </summary>
        public object Key { get; private set; }

        /// <summary>
        /// ソートの方向を指示する値を取得する。
        /// </summary>
        public ListSortDirection Direction { get; private set; }

        public PropertySort(object key, ListSortDirection direction)
        {
            Key = key;
            Direction = direction;
        }
    }
}
