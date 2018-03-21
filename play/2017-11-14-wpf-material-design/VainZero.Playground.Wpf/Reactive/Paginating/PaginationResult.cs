using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Paginating
{
    /// <summary>
    /// ページネーションの結果を表す。
    /// </summary>
    public sealed class PaginationResult<TItem>
        : IPaginationResult
    {
        public SearchResult<TItem> SearchResult { get; private set; }
        public Pager Pager { get; private set; }
        public ImmutableArray<PropertySort> PropertySorts { get; private set; }

        #region IPaginationResult
        IEnumerable IPaginationResult.Items
        {
            get
            {
                return SearchResult.Items;
            }
        }

        int IPaginationResult.TotalCount
        {
            get
            {
                return SearchResult.TotalCount;
            }
        }
        #endregion

        public PaginationResult(SearchResult<TItem> searchResult, Pager pager, ImmutableArray<PropertySort> propertySorts)
        {
            SearchResult = searchResult;
            Pager = pager;
            PropertySorts = propertySorts;
        }

        public static readonly PaginationResult<TItem> Empty =
            new PaginationResult<TItem>(
                SearchResult<TItem>.Empty,
                PagerFactory.Empty,
                ImmutableArray<PropertySort>.Empty
            );
    }

    public interface IPaginationResult
    {
        IEnumerable Items { get; }
        int TotalCount { get; }
        ImmutableArray<PropertySort> PropertySorts { get; }
    }
}
