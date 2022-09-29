namespace AppDesktop
{
    internal sealed class UsersProfilePageVm : BindableBase
    {
        public LoginInfo LoginInfo { get; }

        public EventCommand<object?> GoPasswordChangeCommand { get; }
        public EventCommand<object?> CloseCommand { get; }

        public UsersProfilePageVm(LoginInfo loginInfo)
        {
            LoginInfo = loginInfo;
            GoPasswordChangeCommand = EventCommand.Create<object?>(this);
            CloseCommand = EventCommand.Create<object?>(this);
        }
    }
}
