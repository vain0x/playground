#![allow(unused_imports)]

use builders::Fill;
use builders::{concat, fill, AlignSize, ComputedDoc, Doc, LineKind, CURSOR};
use std;
use std::borrow::Cow;
use std::io::Write;

// FIXME: rename to config
pub struct Options {
    print_width: usize,
    new_line: Option<String>,
}

pub struct Output {
    pub formatted: String,

    // FIXME: in bytes or chars?
    pub cursor: Option<usize>,
}

#[derive(Clone, Copy)]
struct Indent {
    indent: usize,
    spaces: usize,
    tabs: usize,
}

impl std::fmt::Debug for Indent {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        formatter.write_fmt(format_args!(
            "Indent({}, {}, {})",
            self.indent, self.spaces, self.tabs
        ))
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum Mode {
    Break,
    Flat,
}

#[derive(Clone)]
struct Command<'a> {
    indent: Indent,
    mode: Mode,
    doc: &'a Doc,
}

impl<'a> std::fmt::Debug for Command<'a> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        formatter.write_fmt(format_args!(
            "Command({:?} {:?} {:?})",
            self.indent, self.mode, self.doc
        ))
    }
}

fn root_indent() -> Indent {
    Indent {
        indent: 0,
        spaces: 0,
        tabs: 0,
    }
}

fn make_indent(indent: Indent) -> Indent {
    Indent {
        indent: indent.indent + 1,
        ..indent
    }
}

fn make_align(indent: Indent, n: AlignSize) -> Indent {
    match n {
        AlignSize::NegativeInfinity => root_indent(),
        AlignSize::Number(n) => Indent {
            indent: indent.indent,
            spaces: indent.spaces + n,
            tabs: indent.tabs + (if n != 0 { 1 } else { 0 }),
        },
    }
}

fn break_if(mode: Mode, breaks: bool) -> Mode {
    if breaks {
        Mode::Break
    } else {
        mode
    }
}

fn fits(next: Command, rest_commands: &mut Vec<Command>, width: i32, must_be_flat: bool) -> bool {
    // always equal to rest_commands.len() ?
    let mut rest_index = rest_commands.len();
    let mut commands = vec![next];

    let mut width = width;
    while width >= 0 {
        if commands.is_empty() {
            if rest_index == 0 {
                return true;
            }

            commands.push(rest_commands[rest_index - 1].clone());
            rest_index -= 1;
            continue;
        }

        let Command { indent, mode, doc } = commands.pop().unwrap();

        match doc {
            Doc::Source(ref source) => {
                width -= source.len() as i32;
            }
            Doc::Computed(ComputedDoc::Concat { parts }) => {
                for part in parts.iter().rev() {
                    commands.push(Command {
                        indent: indent.clone(),
                        mode,
                        doc: part,
                    })
                }
            }
            Doc::Computed(ComputedDoc::Fill(fill)) => {
                for part in vec![&fill.second, &fill.whitespace, &fill.first] {
                    commands.push(Command {
                        indent: indent.clone(),
                        mode,
                        doc: part,
                    });
                }
            }
            Doc::Computed(ComputedDoc::Indent { contents }) => commands.push(Command {
                indent: make_indent(indent),
                mode,
                doc: contents,
            }),
            Doc::Computed(ComputedDoc::Align { contents, n }) => commands.push(Command {
                indent: make_align(indent, *n),
                mode,
                doc: contents,
            }),
            Doc::Computed(ComputedDoc::Group {
                breaks, contents, ..
            }) => {
                if must_be_flat && *breaks {
                    return false;
                }

                commands.push(Command {
                    indent,
                    mode: break_if(mode, *breaks),
                    doc: contents,
                })
            }
            Doc::Computed(ComputedDoc::IfBreak {
                break_contents,
                flat_contents,
            }) => match (mode, break_contents, flat_contents) {
                (Mode::Break, &Some(ref contents), _) => commands.push(Command {
                    indent,
                    mode,
                    doc: contents,
                }),
                (Mode::Flat, _, &Some(ref contents)) => commands.push(Command {
                    indent,
                    mode,
                    doc: contents,
                }),
                _ => {}
            },
            Doc::Computed(ComputedDoc::Line(kind)) => match (mode, kind) {
                (Mode::Break, _) => return true,
                (Mode::Flat, LineKind::Space) => width -= 1,
                _ => {}
            },
            _ => {}
        }
    }
    false
}

pub fn print_doc_to_string(doc: &Doc, options: &Options) -> Output {
    let (width, new_line) = {
        (
            options.print_width as i32,
            options.new_line.clone().unwrap_or("\n".to_owned()),
        )
    };

    // FIXME
    let tab_width = 4;
    let use_tabs = false;

    let mut pos: i32 = 0;

    // Stack for recursion.
    let mut commands = vec![
        Command {
            indent: root_indent(),
            mode: Mode::Break,
            doc,
        },
    ];

    // what?
    let mut out: Vec<Cow<str>> = Vec::new();
    let mut should_remeasure = false;
    let mut line_suffix: Vec<Command> = Vec::new();

    let err = std::io::stderr();
    let mut lock = err.lock();

    while let Some(Command { indent, mode, doc }) = commands.pop() {
        lock.write_fmt(format_args!("{:?}\n", &Command { indent, mode, doc }))
            .unwrap();

        match (doc, mode) {
            (Doc::Source(ref source), _) => {
                pos += source.len() as i32;
                out.push(Cow::Borrowed(source));
            }
            (Doc::Computed(ComputedDoc::Cursor), _) => unimplemented!(),
            (Doc::Computed(ComputedDoc::Concat { parts }), _) => {
                for part in parts.iter().rev() {
                    commands.push(Command {
                        indent,
                        mode,
                        doc: part,
                    })
                }
            }
            (Doc::Computed(ComputedDoc::Indent { contents }), _) => commands.push(Command {
                indent: make_indent(indent),
                mode,
                doc: contents,
            }),
            (Doc::Computed(ComputedDoc::Align { contents, n }), _) => commands.push(Command {
                indent: make_align(indent, *n),
                mode,
                doc: contents,
            }),
            (
                Doc::Computed(ComputedDoc::Group {
                    breaks, contents, ..
                }),
                Mode::Flat,
            ) if !should_remeasure =>
            {
                commands.push(Command {
                    indent,
                    mode: if *breaks { Mode::Break } else { Mode::Flat },
                    doc: contents,
                })
            }
            (
                Doc::Computed(ComputedDoc::Group {
                    breaks,
                    contents,
                    expanded_states,
                }),
                _,
            ) => {
                should_remeasure = false;

                let next = Command {
                    indent,
                    mode: Mode::Flat,
                    doc: contents,
                };
                let reminder = width - pos;
                if !*breaks && fits(next.clone(), &mut commands, reminder, false) {
                    commands.push(next);
                } else {
                    match expanded_states {
                        Some(_) => unimplemented!(),
                        None => commands.push(Command {
                            indent,
                            mode: Mode::Break,
                            doc: contents,
                        }),
                    }
                }
            }
            (Doc::Computed(ComputedDoc::Fill(fill)), _) => {
                let rem = width - pos;

                let content = &fill.first;
                let content_flat_command = Command {
                    indent,
                    mode: Mode::Flat,
                    doc: content,
                };
                let content_break_command = Command {
                    indent,
                    mode: Mode::Break,
                    doc: content,
                };
                let content_fits = fits(
                    content_flat_command.clone(),
                    &mut Vec::new(),
                    width - rem,
                    true,
                );

                let whitespace = &fill.whitespace;
                let whitespacce_flat_command = Command {
                    indent,
                    mode: Mode::Flat,
                    doc: whitespace,
                };
                let whitespace_break_command = Command {
                    indent,
                    mode: Mode::Break,
                    doc: whitespace,
                };

                let remaining_doc = &fill.second;
                let remaining_command = Command {
                    indent,
                    mode,
                    doc: remaining_doc,
                };

                let two_contents_flat_command = {
                    let second_content = match remaining_doc {
                        Doc::Computed(ComputedDoc::Fill(fill)) => &fill.first,
                        doc => doc,
                    };
                    Command {
                        indent,
                        mode: Mode::Flat,
                        doc: &concat(vec![
                            content.clone(),
                            whitespace.clone(),
                            second_content.clone(),
                        ]),
                    }
                };

                let two_contents_fit = fits(two_contents_flat_command, &mut Vec::new(), rem, true);

                if two_contents_fit {
                    commands.push(remaining_command);
                    commands.push(whitespacce_flat_command);
                    commands.push(content_flat_command);
                } else if content_fits {
                    commands.push(remaining_command);
                    commands.push(whitespace_break_command);
                    commands.push(content_flat_command);
                } else {
                    commands.push(remaining_command);
                    commands.push(whitespace_break_command);
                    commands.push(content_break_command);
                }
            }
            (
                Doc::Computed(ComputedDoc::IfBreak {
                    break_contents: Some(contents),
                    ..
                }),
                Mode::Break,
            ) => commands.push(Command {
                indent,
                mode,
                doc: contents,
            }),
            (
                Doc::Computed(ComputedDoc::IfBreak {
                    flat_contents: Some(contents),
                    ..
                }),
                Mode::Flat,
            ) => commands.push(Command {
                indent,
                mode,
                doc: contents,
            }),
            (Doc::Computed(ComputedDoc::LineSuffix { contents }), _) => line_suffix.push(Command {
                indent,
                mode,
                doc: contents,
            }),
            (Doc::Computed(ComputedDoc::LineSuffixBoundary), _) if !line_suffix.is_empty() => {
                /*
                commands.push(Command {
                    indent,
                    mode,
                    doc: Doc::Computed(ComputedDoc::Line(LineKind::Hard)),
                })
                */
                unimplemented!()
            }
            (Doc::Computed(ComputedDoc::Line(kind)), Mode::Flat)
                if !kind.is_hard() && !kind.is_soft() =>
            {
                out.push(Cow::Borrowed(" "));
                pos += 1;
            }
            (Doc::Computed(ComputedDoc::Line(kind)), Mode::Flat)
                if !kind.is_hard() && kind.is_soft() => {}
            (Doc::Computed(ComputedDoc::Line(kind)), _) => {
                if mode == Mode::Flat {
                    should_remeasure = false;
                }

                if !line_suffix.is_empty() {
                    let mut suffix = Vec::new();
                    std::mem::swap(&mut line_suffix, &mut suffix);

                    for command in suffix.into_iter().rev() {
                        commands.push(command);
                    }
                } else if kind.is_literal() {
                    out.push(Cow::Borrowed(&new_line));
                    pos = 0;
                } else {
                    // FIXME: trim whitespaces

                    let indent_chars = if use_tabs {
                        std::iter::repeat("\t").take(indent.indent + indent.tabs)
                    } else {
                        let len = indent.indent * tab_width + indent.spaces;
                        std::iter::repeat(" ").take(len)
                    };

                    let break_and_indent = std::iter::once(&new_line as &str)
                        .chain(indent_chars)
                        .collect::<String>();
                    pos = break_and_indent.len() as i32;
                    out.push(Cow::Owned(break_and_indent));
                }
            }
            _ => {}
        }
    }

    // FIXME
    // find cursor

    let formatted = out.into_iter().collect::<String>();
    Output {
        formatted,
        cursor: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use builders::{concat, fill, group, AlignSize, ComputedDoc, Doc, LineKind, BREAK_PARENT,
                   CURSOR, LINE};
    use std;
    use std::borrow::Cow;

    #[allow(dead_code)]
    fn print(print_width: usize, doc: &Doc) -> String {
        let options = Options {
            print_width,
            new_line: None,
        };
        let output = print_doc_to_string(doc, &options);
        output.formatted
    }

    #[allow(dead_code)]
    fn s<T: ToString>(source: T) -> Doc {
        Doc::Source(source.to_string())
    }

    #[test]
    fn test_concat_helloworld() {
        let doc = concat(vec![s("hello"), LINE, s("world")]);
        assert_eq!(print(30, &doc), "hello\nworld");
    }

    #[test]
    fn test_concat_and_group() {
        let doc = group(concat(vec![s("hello"), LINE, s("world")]));

        // No break if fit.
        assert_eq!(print(30, &doc), "hello world");

        // Break if not fit.
        assert_eq!(print(10, &doc), "hello\nworld");
    }

    #[test]
    fn test_break_parent() {
        let doc = group(concat(vec![
            s("abcdef"), // 1...6
            LINE,        // 7
            group(concat(vec![
                s("ghi"), // 8..10
                LINE,     // 11
                BREAK_PARENT,
                s("jk"), // 12..13
                LINE,    // 14
                s("l"),  // 15
            ])),
            LINE,       // 16
            s("mnopq"), // 17..21
        ]));

        // If fit.
        assert_eq!(print(80, &doc), "abcdef ghi jk l mnopq");

        // If not fit.
        assert_eq!(print(13, &doc), "abcdef\nghi\njk\nl\nmnopq");
    }

    #[test]
    pub fn test_fill() {
        let words = "Lorem ipsum dolor sit amet".split(" ");
        let doc = fill(
            words
                .flat_map(|word| vec![LINE, s(word)])
                .skip(1)
                .collect::<Vec<_>>(),
        );

        assert_eq!(print(20, &doc), "Lorem ipsum dolor\nsit amet");
    }
}
