namespace AppDesktop
{
    internal sealed class HomePageVm : BindableBase
    {
        public EventCommand<object?> GoEmployeesCommand { get; }

        public HomePageVm()
        {
            GoEmployeesCommand = EventCommand.Create<object?>(this);
        }
    }
}
