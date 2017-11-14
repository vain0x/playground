using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;
using DotNetKit.Paginating;

namespace DotNetKit.Paginating
{
    public sealed class Pager
    {
        public PageNavigationRequest Previous { get; private set; }
        public PageNavigationRequest Next { get; private set; }

        /// <summary>
        /// 特定のページ番号のページに遷移することの要求のリストを取得する。
        /// </summary>
        public ImmutableArray<PageNavigationRequest> NumberRequests { get; private set; }

        /// <summary>
        /// 現在のページのページ番号を取得する。
        /// </summary>
        public int PageIndex { get; private set; }

        /// <summary>
        /// 全体のページ数を取得する。
        /// </summary>
        public int PageCount { get; private set; }

        public int PageIndexPlus1
        {
            get
            {
                return PageIndex + 1;
            }
        }

        public Pager(PageNavigationRequest previous, PageNavigationRequest next, ImmutableArray<PageNavigationRequest> numberRequests, int pageIndex, int pageCount)
        {
            Previous = previous;
            Next = next;
            NumberRequests = numberRequests;
            PageIndex = pageIndex;
            PageCount = pageCount;
        }
    }
}
