# Prettier Printer

This is (nearly) a port of [prettier/prettier-printer] ([version on 2017-01-10]) to learn Rust and pretty printing.

*Not for production.*

Original license is:

> Copyright 2017 James Long
>
> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
>
> The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

[prettier/prettier-printer]: https://github.com/prettier/prettier-printer
[version on 2017-01-10]: https://github.com/prettier/prettier-printer/tree/b57992d1a59b75e66aa18080bce8e965652b4ecf

Currently not supporting:

- group with expanded_states
- conditional_group
- fill
- line_suffix
- hard line
- literal line
- cursor
- utility functions
- etc. (Find by `unimplemented` or `FIXME`.)

