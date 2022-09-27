using System;
using System.Diagnostics;
using System.Windows;
using System.Windows.Controls.Primitives;

namespace AppDesktop
{
    // Thanks to:
    //  "How can I move a WPF Popup when its anchor element moves? - Stack Overflow"
    //  <https://stackoverflow.com/questions/1600218/how-can-i-move-a-wpf-popup-when-its-anchor-element-moves>

    /// <see cref="UseProperty" />
    internal static class PopupRelocationBehavior
    {
        /// <summary>
        /// Make the popup to be relocated automatically.
        ///
        /// <para>
        /// Usage in XAML:
        /// <code><![CDATA[<Popup ns:PopupRelocationBehavior.Use="True"/>]]></code>
        /// </para>
        /// 
        /// <para>
        /// Property <code>Use</code> can't be false.
        /// </para>
        /// </summary>
        public static DependencyProperty UseProperty { get; } = DependencyProperty.RegisterAttached(
            "Use",
            typeof(bool),
            typeof(PopupRelocationBehavior),
            new FrameworkPropertyMetadata
            {
                DefaultValue = false,
                PropertyChangedCallback = OnUseChanged,
            });

        public static bool GetUse(DependencyObject obj) => (bool)obj.GetValue(UseProperty);
        public static void SetUse(DependencyObject obj, bool value) => obj.SetValue(UseProperty, value);

        private static void OnUseChanged(DependencyObject obj, DependencyPropertyChangedEventArgs e)
        {
            if (obj is not Popup popup)
                throw new ArgumentException($"{nameof(PopupRelocationBehavior)}.Use can be attached to a Popup object.");

            if ((bool?)e.NewValue != true)
                throw new ArgumentException($"{nameof(PopupRelocationBehavior)}.Use must be true.");

            // The popup isn't loaded yet if `Use=True` is set on construction.
            Debug.Assert(!popup.IsLoaded);

            popup.Loaded += OnPopupLoaded;
        }

        private static void OnPopupLoaded(object sender, RoutedEventArgs _e)
        {
            Debug.WriteLine("OnPopupLoaded");
            Popup popup = (Popup)sender;

            var w = Window.GetWindow(popup);
            if (w == null) return;

            // We're attaching event handlers to the window.
            // Be sure to detach them to avoid resource leak.
            // (Notice that window events hold references to the popup through these handlers,
            //  which would make the lifetime of the popup longer than necessary.
            //  On the other hand, `Popup.Unloaded` handler doesn't need to be deatched.)

            var onLocationChanged = new EventHandler((_, _) => ForceRelocate(popup));
            var onSizeChanged = new SizeChangedEventHandler((_, _) => ForceRelocate(popup));

            Debug.WriteLine("PopupRelocationBehavior: Attach window events");
            w.LocationChanged += onLocationChanged;
            w.SizeChanged += onSizeChanged;

            popup.Unloaded += (_, _) =>
            {
                Debug.WriteLine("PopupRelocationBehavior: Detach window events");
                w.LocationChanged -= onLocationChanged;
                w.SizeChanged -= onSizeChanged;
            };
        }

        private static void ForceRelocate(Popup popup)
        {
            var offset = popup.HorizontalOffset;
            popup.HorizontalOffset = offset + 1;
            popup.HorizontalOffset = offset;
        }
    }
}
