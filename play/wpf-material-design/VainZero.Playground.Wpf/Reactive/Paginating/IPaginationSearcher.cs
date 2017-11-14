using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Paginating
{
    public interface IPaginationSearcher<TItem>
    {
        /// <summary>
        /// ページネーションやソートに関する検索条件を取得する。
        /// </summary>
        PaginationCondition PaginationCondition { get; }

        /// <summary>
        /// 現在の設定で検索を行なう。
        /// </summary>
        SearchResult<TItem> Search();
    }
}
