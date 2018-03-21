using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using Reactive.Bindings;

namespace DotNetKit.Paginating
{
    public sealed class PagerFactory
    {
        /// <summary>
        /// 特定のページに遷移する要求の最大数。
        /// </summary>
        private static readonly int NumberRequestCount = 7;

        private static int PageCount(int total, int limit)
        {
            if (total == 0 || limit == 0) return 1;
            return (total + limit - 1) / limit;
        }

        private static int Clamp(int value, int min, int max)
        {
            return Math.Max(min, Math.Min(max, value));
        }

        public Pager Create(int currentIndex, int limit, int totalCount)
        {
            var pageCount = PageCount(totalCount, limit);
            currentIndex = Clamp(currentIndex, 0, pageCount - 1);

            var createRequest =
                new Func<int, PageNavigationRequest>(pageIndex =>
                    new PageNavigationRequest(
                        pageIndex,
                        pageCount,
                        isCurrent: pageIndex == currentIndex
                    ));

            var count = Math.Min(pageCount, NumberRequestCount);
            var first = Clamp(currentIndex - count / 2, 0, pageCount - count);
            var numberRequests =
                Enumerable.Range(first, count)
                .Select(createRequest)
                .ToImmutableArray();
            return
                new Pager(
                    createRequest(currentIndex - 1),
                    createRequest(currentIndex + 1),
                    numberRequests,
                    currentIndex,
                    pageCount
                );
        }

        public static readonly Pager Empty =
            new Pager(
                new PageNavigationRequest(-1, 1, false),
                new PageNavigationRequest(1, 1, false),
                ImmutableArray<PageNavigationRequest>.Empty,
                pageIndex: 0,
                pageCount: 1
            );
    }
}
