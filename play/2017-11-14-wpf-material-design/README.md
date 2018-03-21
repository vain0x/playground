---
tag: \#wip \#sketch \#skeleton \#dotnet \#csharp \#wpf \#mvvm
---

# WPF Sample App

Sample of GUI app on Windows .NET Framework using:

- [Prism.Mvvm](https://github.com/PrismLibrary/Prism)
    - MVVM library.
- [MaterialDesignInXamlToolkit](https://github.com/ButchersBoy/MaterialDesignInXamlToolkit)
    - Material Design UI Framework

## Utilities

- ``VainZero.Playground.Wpf``/
    - ``Reactive``/
        - ``Collections``/
            - ``ReactiveArray.cs`` :
                An observable collection wrapping an immutable array.
                - Replaces the inner ImmutableArray on update, notifying `INotifyCollectionChanged` events.
                - Provides exception-safety and good performance in a particular situation.
        - ``Paginating``/ : DataGrid extension supporting pagination, sorting and dynamic update.
    - ``DialogHostSampleControl.xaml`` :
        An idea of truly-MVVM wrapper for `DialogHost` (from XAML toolkit). Still needs refactoring and abstraction.
