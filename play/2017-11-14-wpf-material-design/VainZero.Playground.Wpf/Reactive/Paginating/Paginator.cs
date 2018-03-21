using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Reactive.Bindings;
using System.Reactive.Subjects;
using System.Windows.Input;
using DotNetKit.Reactive.Commands;
using System.Collections.Immutable;
using System.ComponentModel;
using System.Collections;
using System.Collections.ObjectModel;
using DotNetKit.Reactive.Collections;

namespace DotNetKit.Paginating
{
    /// <summary>
    /// ページネーターを表す。
    /// </summary>
    public sealed class Paginator<TItem>
        : IPaginator
    {
        private readonly IPaginationSearcher<TItem> _searcher;

        private readonly PagerFactory _pagerFactory = new PagerFactory();

        private PaginationResult<TItem> _current;

        /// <summary>
        /// ページネーションの最新の結果を取得する。
        /// </summary>
        public PaginationResult<TItem> Current
        {
            get
            {
                return _current;
            }
        }

        private SearchResult<TItem> SearchSafe()
        {
            if (_searcher.PaginationCondition.Limit <= 0)
            {
                return SearchResult<TItem>.Empty;
            }

            return _searcher.Search();
        }

        private PaginationResult<TItem> Paginate(SearchResult<TItem> searchResult)
        {
            var pc = _searcher.PaginationCondition;
            var pager =
                _pagerFactory.Create(
                    currentIndex: pc.PageIndex,
                    limit: pc.Limit,
                    totalCount: searchResult.TotalCount
                );
            var propertySorts =
                pc.PropertySorts.ToImmutableArray();
            return new PaginationResult<TItem>(searchResult, pager, propertySorts);
        }

        /// <summary>
        /// 現在の条件で再検索する。
        /// </summary>
        public void Search()
        {
            _current = Paginate(SearchSafe());
            RaiseNotifyPropertyChanged(CurrentPropertyChangedEventArg);
        }

        /// <summary>
        /// 指定されたキーに関するソート条件を更新して、再検索する。
        /// </summary>
        public void Sort(object sortKey, bool reset)
        {
            _searcher.PaginationCondition.UpdateSortKey(sortKey, reset);
            Search();
        }

        /// <summary>
        /// 指定されたページ番号に遷移する。
        /// </summary>
        private void Navigate(PageNavigationRequest request)
        {
            _searcher.PaginationCondition.PageIndex = request.PageIndex;
            Search();
        }

        public ICommand<PageNavigationRequest> NavigateCommand { get; private set; }

        #region INotifyPropertyChanged
        public event PropertyChangedEventHandler PropertyChanged;

        private void RaiseNotifyPropertyChanged(PropertyChangedEventArgs e)
        {
            var h = PropertyChanged;
            if (h != null) h(this, e);
        }

        private static readonly PropertyChangedEventArgs CurrentPropertyChangedEventArg =
            new PropertyChangedEventArgs("Current");
        #endregion

        #region IPaginator
        IPaginationResult IPaginator.Current
        {
            get
            {
                return Current;
            }
        }

        IReactiveArray IPaginator.CreateReactiveList()
        {
            return new ReactiveArray<TItem>(ImmutableArray<TItem>.Empty);
        }
        #endregion

        public Paginator(IPaginationSearcher<TItem> searcher)
        {
            _searcher = searcher;

            _current = PaginationResult<TItem>.Empty;

            NavigateCommand = RaisableCommand.Create<PageNavigationRequest>(Navigate, r => r.IsValid);
        }
    }

    public interface IPaginator
        : INotifyPropertyChanged
    {
        IPaginationResult Current { get; }

        ICommand<PageNavigationRequest> NavigateCommand { get; }

        void Sort(object sortKey, bool reset);

        /// <summary>
        /// 型引数として特定の型が与えられた <see cref="IReactiveArray{T}"/> を生成する。
        /// NOTE: DataGrid.ItemsSource に IList[T] を渡すと、要素数がゼロでも型 T に基いてカラムが自動生成される。(T の代わりに object にするとダメ。) これが型引数を知っているのでここで注入しておく。
        /// </summary>
        IReactiveArray CreateReactiveList();
    }

    public static class Paginator
    {
        public static Paginator<T> Create<T>(IPaginationSearcher<T> searcher)
        {
            return new Paginator<T>(searcher);
        }
    }
}
