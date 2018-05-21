use std::collections::VecDeque;
use std::ops;
use std::cell::Cell;
use std::fmt;
use std::io::{self,Write};

use lexer::{Span,SpanData};

use instructions::SizeHint;

use byteorder::{WriteBytesExt,LittleEndian};

#[derive(Debug)]
pub enum ArgumentKind {
    Implied,
    Constant,
    Direct,
    IndexedX,
    IndexedY,
    Indirect,
    IndX,
    IndY,
    IndLong,
    IndLongY,
    Stack,
    StackY,
    TwoArgs(Span,Span)  // deal with this separately
}

#[derive(Debug)]
pub struct Argument {
    pub kind: ArgumentKind,
    pub span: Span
}



impl Argument {
    fn implied() -> Self {
        Argument { kind: ArgumentKind::Implied, span: Span::Empty }
    }
    fn parse(spans: &mut [Span]) -> Result<Self, ParseError> {
        use self::Span::*;
        use self::ArgumentKind::*;
        //spans.iter().for_each(|s| println!("{}", s));
        let empty = &[Span::Empty][..];
        // todo: add expression parsing?
        let (kind, spans) = match spans {
            [] => (Implied, empty),
            [ref c.., Symbol(',',_), Ident(ref x)] if x.data == "x" => (IndexedX, c),
            [ref c.., Symbol(',',_), Ident(ref y)] if y.data == "y" => (IndexedY, c),
            [Symbol('(',_), ref c.., Symbol(',',_), Ident(ref x), Symbol(')',_)] if x.data == "x" => (IndX, c),
            [Symbol('(',_), ref c.., Symbol(')',_), Symbol(',',_), Ident(ref y)] if y.data == "y" => (IndY, c),
            [Symbol('[',_), ref c.., Symbol(']',_)] => (IndLong, c),
            [Symbol('[',_), ref c.., Symbol(']',_), Symbol(',',_), Ident(ref y)] if y.data == "y" => (IndLongY, c),
            [ref c.., Symbol(',',_), Ident(ref s)] if s.data == "s" => (Stack, c),
            [Symbol('(',_), ref c.., Symbol(',',_), Ident(ref s), Symbol(')',_), Symbol(',',_), Ident(ref y)] if s.data == "s" && y.data == "y" => (StackY, c),
            [Symbol('(',_), ref c.., Symbol(')',_)] => (Indirect, c),
            // TODO: properly fix block syntax
            [Symbol('#',_), ref c1, Symbol(',',_), Symbol('#',_), ref c2] => (TwoArgs(c1.clone(), c2.clone()), empty),
            [ref c1, Symbol(',',_), ref c2] => (TwoArgs(c1.clone(), c2.clone()), empty),
            [Symbol('#',_), ref c..] => (Constant, c),
            [ref c..] => (Direct, c),
        };
        // Potentially avoid cloning? Mostly numbers anyway though so
        let span = Span::coagulate(spans);
        Ok(Argument { kind, span })
    }
}

#[derive(Debug)]
pub enum Statement {
    Label {
        name: Span
    },
    LocalLabel {
        depth: usize,
        name: Span
    },
    Instruction {
        name: Span,
        size: (SizeHint,Option<Span>),
        arg: Argument
    },
    RawData {
        data: Vec<u8>,
    },
    Attributes {
        attrs: Vec<Span>
    }
}
/*
impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match self {
            Label { name } => write!(f, "Label {}", name),
            LocalLabel { depth, name } => write!(f, "Local label {}", name),
            Instruction { name, size, arg } => write!(f, "Instruction: {}, argument: {:?}", name, arg),
            Attributes { attrs } => write!(f, "Attributes (unused yet)"),
            RawData { data } => {
                write!(f, "Raw data: ")?;
                for i in data { write!(f, "{:02X} ", i)? }
                Ok(())
            },
            _ => write!(f, "Unimplemented!")
        }
    }
}
*/

#[derive(Debug)]
pub enum ParseError {
    UnknownSpan,
    InvalidOpSize(Span),
    GenericSyntaxError,
    UnknownAddressingMode(Span),
    UnexpectedSymbol(Span)
}

// convenience for clearing the struct
struct QueueClearHandle<'a,T: 'a>(&'a mut Vec<T>);
impl<'a,T> Drop for QueueClearHandle<'a,T> {
    fn drop(&mut self) {
        self.0.clear();
    }
}
impl<'a,T> ops::Deref for QueueClearHandle<'a,T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Vec<T> {
        &*self.0
    }
}
impl<'a,T> ops::DerefMut for QueueClearHandle<'a,T> {
    fn deref_mut(&mut self) -> &mut Vec<T> {
        self.0
    }
}

pub struct Parser<S> {
    iter: S,
    buf: Vec<Span>
}
impl<S: Iterator<Item=Span>> Parser<S> {
    pub fn new(iter: S) -> Self {
        Self { iter, buf: Default::default() }
    }
}
impl<S: Iterator<Item=Span>> Iterator for Parser<S> {
    type Item = Result<Statement,ParseError>;
    fn next(&mut self) -> Option<Result<Statement,ParseError>> {
        use self::Span::*;
        use self::Statement::*;
        let mut buf = QueueClearHandle(&mut self.buf);
        let iter = &mut self.iter;
        let mut clear = false;
        let res = (|| loop {
            if clear {
                buf.clear();
                clear = false;
            }
            let c = if let Some(c) = iter.next() { c } else {
                // The stream has ended but we can't parse the end
                if buf.len() > 0 {
                    return Err(ParseError::GenericSyntaxError)
                } else {
                    return Ok(None)
                }
            };
            buf.push(c);
            return Ok(Some(match &mut **buf {
                // Label:
                [ref mut name @ Ident(_), Symbol(':',_)] |
                [ref mut name @ PosLabel(_)] | [ref mut name @ NegLabel(_)] =>
                    Label { name: name.take() },
                // closure below is a workaround
                [ref mut dots.., ref mut name @ Ident(_), Symbol(':',_)]
                    if (|| dots.iter().all(|c| if let Symbol('.',_) = c { true } else { false }))() =>
                        LocalLabel { depth: dots.len() - 1, name: name.take() },
                [Ident(ref c), ref rest.., LineBreak] if c.data == "db" => RawData {
                    data: {
                        let mut dbuf = Vec::new();
                        let mut allow_comma = false;
                        // Allow any kind unless annotated as strict? TODO: local or global config based on attrs
                        for c in rest.iter() {
                            match c {
                                Whitespace => {},
                                Symbol(',',_) if allow_comma => allow_comma = false,
                                Byte(c) => { allow_comma = true; dbuf.write_u8(c.data as u8).unwrap(); },
                                Word(c) => { allow_comma = true; dbuf.write_u16::<LittleEndian>(c.data as u16).unwrap(); },
                                Long(c) => { allow_comma = true; dbuf.write_u24::<LittleEndian>(c.data as u32).unwrap(); },
                                c => return Err(ParseError::UnexpectedSymbol(c.clone()))
                            }
                        }
                        dbuf
                    }
                },
                [ref mut name @ Ident(_), LineBreak] |
                [ref mut name @ Ident(_), Whitespace, Symbol(':',_)] => Instruction {
                    name: name.take(),
                    size: Default::default(),
                    arg: Argument::parse(&mut []).unwrap()
                },
                [ref mut name @ Ident(_), Whitespace, ref mut rest.., LineBreak] |
                [ref mut name @ Ident(_), Whitespace, ref mut rest.., Whitespace, Symbol(':',_)] => Instruction {
                    name: name.take(),
                    size: Default::default(),
                    arg: Argument::parse(rest)?
                },
                [ref mut name @ Ident(_), Symbol('.',_), ref mut size @ Ident(_), Whitespace, ref mut rest.., LineBreak] |
                [ref mut name @ Ident(_), Symbol('.',_), ref mut size @ Ident(_), Whitespace, ref mut rest.., Whitespace, Symbol(':',_)]
                    => Instruction {
                    name: name.take(),
                    size: (SizeHint::parse(size.as_ident().unwrap()).ok_or(ParseError::GenericSyntaxError)?, Some(size.take())),
                    arg: Argument::parse(rest)?
                },
                [Symbol('#',_), Symbol('[',_), ref rest.., Symbol(']',_)] => Attributes {
                    attrs: rest.to_vec()
                },
                [Whitespace] | [LineBreak] => { clear = true; continue; },
                [.., LineBreak] => return Err(ParseError::GenericSyntaxError),
                _ => continue
            }));
        })();
        match res {
            Ok(Some(c)) => Some(Ok(c)),
            Ok(None) => None,
            Err(e) => Some(Err(e))
        }
    }
}
