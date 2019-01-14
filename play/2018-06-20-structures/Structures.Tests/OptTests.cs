using System;
using System.Collections.Generic;
using Xunit;

namespace Structures
{
    public class OptTests
    {
        [Fact]
        public void SomeHasValue()
        {
            var some1 = Opt.Some(1);

            some1.IsSome
                .Is(true);
            some1.Value
                .Is(1);
        }

        [Fact]
        public void NoneHasNoValue()
        {
            var none = Opt.None<int>();

            none.IsSome
                .Is(false);

            Assert.Throws<InvalidOperationException>(() =>
                Opt.None<string>().Value
            );
        }

        [Fact]
        public void SomeRejectsNull()
        {
            Assert.Throws<ArgumentNullException>(() =>
                Opt.Some((string)null)
            );
        }

        public class NotNullTests
        {
            [Fact]
            public void ItMapsNullReferenceToNone()
            {
                Opt.NotNull((string)null)
                    .Is(Opt.None<string>());
            }

            [Fact]
            public void ItMapsNullStructToNone()
            {
                Opt.NotNull((int?)null)
                    .Is(Opt.None<int?>());
            }

            [Fact]
            public void ItMapsNonnullToSome()
            {
                Opt.NotNull("not null")
                    .Is(Opt.Some("not null"));
            }

            [Fact]
            public void ItMapsNonnullDefaultToSome()
            {
                Opt.NotNull(default(int))
                    .Is(Opt.Some(0));
            }

            [Fact]
            public void ItMapsNoneToNestedSome()
            {
                Opt.NotNull(Opt.None<int>())
                    .Is(Opt.Some(Opt.None<int>()));
            }
        }
    }
}
