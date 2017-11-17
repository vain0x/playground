using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Playground
{
    // -----------------------------------------------------------------------
    // user code

    /// <summary>
    /// Represents immutable data context for the authentication page.
    /// Written by hand.
    /// </summary>
    public sealed partial class AuthenticationPageViewModel
        : IMainSurfacePageViewModel
        , IViewModel
    {
        public object Identity => _id;

        private long _id;

        public string Email { get; }
        public string EmailError { get; }

        //public string Password { get; }
        //public string PasswordError { get; }

        public AuthenticationPageViewModel WithEmail(string value)
        {
            // Validate email.
            var error = value.Contains("@") ? "OK" : "Not an email.";

            return new AuthenticationPageViewModel(_id, value, error);
        }

        /// <summary>Record Constructor</summary>
        /// <param name="email"><see cref="Email"/></param>
        /// <param name="emailError"><see cref="EmailError"/></param>
        public AuthenticationPageViewModel(long id, string email, string emailError)
        {
            _id = id;
            Email = email;
            EmailError = emailError;
        }
    }

    public sealed class AuthenticationPageViewModelUpdater
    {
        public AuthenticationPageViewModel Update(AuthenticationPageViewModel page, PropertyChange change)
        {
            throw new NotImplementedException();
        }
    }

    public sealed partial class MainSurfaceFrameViewModel
        : IViewModelRoot
    {
        public MainSurfaceFrameViewModel(IMainSurfacePageViewModel content)
        {
            Content = content;
        }

        public object Identity => 0;

        public IMainSurfacePageViewModel Content { get; }
    }

    public interface IMainSurfacePageViewModel
        : IViewModel
    {
    }

    // ------------------------------------------------------------------------
    // framework layer

    public interface IViewModel
    {
        object Identity { get; }

        IViewModelMutable CreateMutable(IViewModelMutable parent, IViewModelProperty property);
    }

    public interface IViewModelRoot
        : IViewModel
    {
    }

    public interface IViewModelReducer<TImmutable, TMutable>
    {
        TImmutable Reduce(TMutable mutable);
    }

    public interface IViewModelProperty
    {
        object GetImmutable(object obj);
        object GetMutable(object obj);
    }

    public sealed class ViewModelProperty
        : IViewModelProperty
    {
        private Func<object, object> _getImmutable, _getMutable;

        public ViewModelProperty(Func<object, object> getImmutable, Func<object, object> getMutable)
        {
            _getImmutable = getImmutable;
            _getMutable = getMutable;
        }

        public object GetImmutable(object obj) => _getImmutable(obj);
        public object GetMutable(object obj) => _getMutable(obj);
    }

    public struct PropertyChange
    {
        public IViewModelMutable ViewModel { get; }
        public IViewModelProperty Property { get; }
        public object NewValue { get; }
        /// <summary>Record Constructor</summary>
        /// <param name="viewModel"><see cref="ViewModel"/></param>
        /// <param name="property"><see cref="Property"/></param>
        /// <param name="oldValue"><see cref="OldValue"/></param>
        /// <param name="newValue"><see cref="NewValue"/></param>
        public PropertyChange(IViewModelMutable viewModel, IViewModelProperty property, object newValue)
        {
            ViewModel = viewModel;
            Property = property;
            NewValue = newValue;
        }
    }

    public sealed class ReduceRequest
    {
        public PropertyChange[] Changes { get; }
        /// <summary>Record Constructor</summary>
        /// <param name="changes"><see cref="Changes"/></param>
        public ReduceRequest(PropertyChange[] changes)
        {
            Changes = changes;
        }
    }

    public interface IViewModelMutable
        : INotifyPropertyChanged
    {
        IViewModelMutable Parent { get; set; }
        IViewModel State { get; set; }

        // Parent からみた、このオブジェクトの property
        IViewModelProperty StateProperty { get; set; }

        void ApplyChange(IViewModel stateObject, IEnumerable<PropertyChange> rest);
    }

    public interface IViewModelRootMutable
        : IViewModelMutable
    {
        ICommand<ReduceRequest> ReduceCommand { get; set; }
    }

    public static class ViewModelMutableExtension
    {
        public static ICommand<ReduceRequest> FindReduceCommand(this IViewModelMutable @this)
        {
            var node = @this;
            while (node.Parent != null)
            {
                node = node.Parent;
            }
            return ((IViewModelRootMutable)node).ReduceCommand;
        }

        public static IViewModelMutable[] NodesFromRootToSelf(this IViewModelMutable @this)
        {
            var list = new List<IViewModelMutable>();
            var node = @this;
            while (node != null)
            {
                list.Add(node);
                node = node.Parent;
            }
            list.Reverse();
            return list.ToArray();
        }

        public static void OnSet(this IViewModelMutable @this, IViewModelProperty property, object value)
        {
            PropertyChange[] changes;
            {
                var list = new List<PropertyChange>();
                var node = @this;
                var p = property;
                var v = value;
                while (node != null)
                {
                    list.Add(new PropertyChange(node, p, v));
                    p = node.StateProperty;
                    v = node;
                    node = node.Parent;
                }
                list.Reverse();
                changes = list.ToArray();
            }

            var request = new ReduceRequest(changes);
            var root = (IViewModelRootMutable)(changes[0].ViewModel);
            root.ReduceCommand.Execute(request);
        }
    }

    // ------------------------------------------------------------------------
    // auto-gen

    // dependency property として定義したほうがパフォーマンスがよい

    public partial class MainSurfaceFrameViewModel
    {
        public static ViewModelProperty ContentProperty { get; } =
            new ViewModelProperty(
                r => ((MainSurfaceFrameViewModel)r).Content,
                r => ((MainSurfaceFrameViewModelMutable)r).Content
            );

        public IViewModelMutable CreateMutable(IViewModelMutable parent, IViewModelProperty property)
        {
            var m = new MainSurfaceFrameViewModelMutable();
            m.Parent = parent;
            m.State = this;
            m.StateProperty = property;
            m.Content = Content.CreateMutable(m, ContentProperty);
            m.ReduceCommand = RaisableCommand.Create<ReduceRequest>(r =>
            {
                m.ApplyChange((IViewModel)r.Changes[0].NewValue, r.Changes.Skip(1));
            }, _ => true);
            return m;
        }
    }

    public sealed class MainSurfaceFrameViewModelMutable
        : IViewModelRootMutable
    {
        public IViewModelMutable Parent { get; set; }

        public IViewModel State { get; set; }

        public IViewModelProperty StateProperty { get; set; }

        public event PropertyChangedEventHandler PropertyChanged;

        public ICommand<ReduceRequest> ReduceCommand { get; set; }

        IViewModelMutable _content;
        public IViewModelMutable Content
        {
            get => _content;
            set => this.OnSet(MainSurfaceFrameViewModel.ContentProperty, value);
        }

        public void ApplyChange(IViewModel stateObject, IEnumerable<PropertyChange> rest)
        {
            var state = (MainSurfaceFrameViewModel)stateObject;

            {
                if (_content.State.Identity == state.Identity)
                {
                    _content.ApplyChange(state.Content, rest);
                }
                else
                {
                    _content = state.CreateMutable(this, MainSurfaceFrameViewModel.ContentProperty);
                    PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(nameof(Content)));
                }
            }
        }
    }

    public sealed class AuthenticationPageViewModelMutable
        : IViewModelMutable
    {
        public IViewModelMutable Parent { get; set; }

        public IViewModel State { get; set; }

        public IViewModelProperty StateProperty { get; set; }

        public event PropertyChangedEventHandler PropertyChanged;

        string _email;
        public string Email
        {
            get
            {
                return _email;
            }
            set
            {
                this.OnSet(AuthenticationPageViewModel.EmailProperty, value);
            }
        }

        public string EmailError { get; set; }

        public void ApplyChange(IViewModel stateObject, IEnumerable<PropertyChange> rest)
        {
            var state = (AuthenticationPageViewModel)stateObject;

            if (!_email.EqualsGeneric(state.Email))
            {
                _email = state.Email;
                PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(nameof(Email)));
            }
        }
    }

    public partial class AuthenticationPageViewModel
    {
        public static ViewModelProperty EmailProperty { get; } =
            new ViewModelProperty(
                r => ((AuthenticationPageViewModel)r).Email,
                r => ((AuthenticationPageViewModelMutable)r).Email
            );

        public static ViewModelProperty EmailErrorProperty { get; } =
            new ViewModelProperty(
                r => ((AuthenticationPageViewModel)r).EmailError,
                r => ((AuthenticationPageViewModelMutable)r).EmailError
            );

        public IViewModelMutable CreateMutable(IViewModelMutable parent, IViewModelProperty property)
        {
            var m = new AuthenticationPageViewModelMutable();
            m.Parent = parent;
            m.State = this;
            m.StateProperty = property;
            m.Email = Email;
            m.EmailError = EmailError;
            return m;
        }
    }
}
