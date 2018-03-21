using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Optional;
using Optional.Collections;
using Optional.Unsafe;
using DotNetKit.Optional;

namespace DotNetKit.Paginating
{
    /// <summary>
    /// ページネーションとソートに関する条件を表す。
    /// </summary>
    public sealed class PaginationCondition
    {
        /// <summary>
        /// 対象のページ番号を取得・設定する。
        /// </summary>
        public int PageIndex { get; set; }

        /// <summary>
        /// 1ページあたりの要素数の上限を取得・設定する。
        /// </summary>
        public int Limit { get; set; }

        /// <summary>
        /// ソートの条件を取得する。
        /// </summary>
        public List<PropertySort> PropertySorts { get; private set; }

        private static bool EqualsGeneric<X>(X left, X right)
        {
            return EqualityComparer<X>.Default.Equals(left, right);
        }

        /// <summary>
        /// ソートの方向を「なし→昇順→降順→なし」の順番で置換する。
        /// </summary>
        private static Option<ListSortDirection> Next(Option<ListSortDirection> direction)
        {
            switch (direction.ToNullable())
            {
                case null:
                    return ListSortDirection.Ascending.Some();
                case ListSortDirection.Ascending:
                    return ListSortDirection.Descending.Some();
                case ListSortDirection.Descending:
                    return Option.None<ListSortDirection>();
                default:
                    throw new ArgumentException("direction");
            }
        }

        /// <summary>
        /// 指定されたキーに関するソート条件を次の状態に進める。
        /// <paramref name="reset"/> が指定された場合、他のソート条件は消去する。
        /// </summary>
        public void UpdateSortKey(object sortKey, bool reset)
        {
            // 指定されたカラムが現在のソート条件の何番目に出現するか。
            var indexOption =
                PropertySorts.FindIndexOrNone(ps => EqualsGeneric(ps.Key, sortKey));

            // 指定されたカラムの更新後のソート条件。
            var newPropertySort =
                Next(indexOption.Map(i => PropertySorts[i].Direction))
                .Map(d => new PropertySort(sortKey, d));

            indexOption.Match(
                i =>
                {
                    newPropertySort.Match(
                        ps => PropertySorts[i] = ps,
                        () => PropertySorts.RemoveAt(i)
                    );
                },
                () =>
                {
                    if (reset) PropertySorts.Clear();
                    PropertySorts.AddRange(newPropertySort.ToEnumerable());
                });
        }

        public PaginationCondition(int pageIndex, int limit, IEnumerable<PropertySort> propertySorts)
        {
            PageIndex = pageIndex;
            Limit = limit;
            PropertySorts = propertySorts.ToList();
        }

        public static PaginationCondition Create()
        {
            return
                new PaginationCondition(
                    pageIndex: 0,
                    limit: 0,
                    propertySorts: Array.Empty<PropertySort>()
                );
        }
    }
}
