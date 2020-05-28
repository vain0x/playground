using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Media;

namespace VainZero.Windows.Media
{
    public static class VisualExtension
    {
        public static IEnumerable<DependencyObject>
            VisualChildren(this DependencyObject obj)
        {
            var count = VisualTreeHelper.GetChildrenCount(obj);
            for (var i = 0; i < count; i++)
            {
                yield return VisualTreeHelper.GetChild(obj, i);
            }
        }

        public static IEnumerable<DependencyObject>
            VisualDescendantsBreadthFirstOrder(this DependencyObject obj)
        {
            var queue = new Queue<DependencyObject>();
            queue.Enqueue(obj);

            while (queue.Count > 0)
            {
                var it = queue.Dequeue();
                yield return it;

                foreach (var child in it.VisualChildren())
                {
                    queue.Enqueue(child);
                }
            }
        }
    }
}
