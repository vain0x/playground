using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Paginating
{
    public sealed class SearchResult<T>
    {
        public ImmutableArray<T> Items { get; private set; }
        public int TotalCount { get; private set; }

        public SearchResult(ImmutableArray<T> items, int totalCount)
        {
            Items = items;
            TotalCount = totalCount;
        }

        public static readonly SearchResult<T> Empty =
            new SearchResult<T>(ImmutableArray<T>.Empty, totalCount: 0);
    }
}
