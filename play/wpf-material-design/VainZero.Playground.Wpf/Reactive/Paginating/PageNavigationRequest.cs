using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Paginating
{
    /// <summary>
    /// <see cref="Paginator{TItem}"/> へのページ遷移の要求を表す。
    /// </summary>
    public sealed class PageNavigationRequest
    {
        private readonly int _pageCount;

        /// <summary>
        /// 遷移先のページ番号を取得する。
        /// </summary>
        public int PageIndex { get; private set; }

        /// <summary>
        /// 遷移先のページ番号が現在のページ番号と等しい場合に真を取得する。
        /// </summary>
        public bool IsCurrent { get; private set; }

        public int PageIndexPlus1
        {
            get
            {
                return PageIndex + 1;
            }
        }

        public bool IsValid
        {
            get
            {
                return !IsCurrent && (0 <= PageIndex && PageIndex < _pageCount);
            }
        }

        public PageNavigationRequest(int pageIndex, int pageCount, bool isCurrent)
        {
            PageIndex = pageIndex;
            _pageCount = pageCount;
            IsCurrent = isCurrent;
        }
    }
}
