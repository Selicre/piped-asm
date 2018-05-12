use std::collections::VecDeque;
use std::ops;
use std::cell::Cell;
use std::fmt;
use std::io::{self,Write};

use lexer::{Span,SpanData};

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
    // TODO: make this less stupid, pass the relevant spanqueue part instead
    fn parse(spans: &[Span]) -> Result<Self, ParseError> {
        use self::Span::*;
        use self::ArgumentKind::*;
        //spans.iter().for_each(|s| println!("{}", s));
        // Potentially avoid cloning? Mostly numbers anyway though so
        let (kind, span) = match spans {
            &[] => (Implied, Empty),
            &[ref c] => (Direct, c.clone()),
            &[Symbol('#',_), ref c] => (Constant, c.clone()),
            &[ref c, Symbol(',',_), Ident(ref x)] if x.data == "x" => (IndexedX, c.clone()),
            &[ref c, Symbol(',',_), Ident(ref y)] if y.data == "y" => (IndexedY, c.clone()),
            &[Symbol('(',_), ref c, Symbol(')',_)] => (Indirect, c.clone()),
            &[Symbol('(',_), ref c, Symbol(',',_), Ident(ref x), Symbol(')',_)] if x.data == "x" => (IndX, c.clone()),
            &[Symbol('(',_), ref c, Symbol(')',_), Symbol(',',_), Ident(ref y)] if y.data == "y" => (IndY, c.clone()),
            &[Symbol('[',_), ref c, Symbol(']',_)] => (IndLong, c.clone()),
            &[Symbol('[',_), ref c, Symbol(']',_), Symbol(',',_), Ident(ref y)] if y.data == "y" => (IndLongY, c.clone()),
            &[ref c, Symbol(',',_), Ident(ref s)] if s.data == "s" => (Stack, c.clone()),
            &[Symbol('(',_), ref c, Symbol(',',_), Ident(ref s), Symbol(')',_), Symbol(',',_), Ident(ref y)] if s.data == "s" && y.data == "y" => (StackY, c.clone()),
            &[Symbol('#',_), ref c1, Symbol(',',_), Symbol('#',_), ref c2] => (TwoArgs(c1.clone(), c2.clone()), Empty),
            &[ref c1, Symbol(',',_), ref c2] => (TwoArgs(c1.clone(), c2.clone()), Empty),
            c => return Err(ParseError::UnknownAddressingMode(c[0].clone()))
        };
        Ok(Argument { kind, span })
    }
}
#[derive(Debug,Clone)]
pub enum OpSize {
    Byte,
    Word,
    Long,
}
impl OpSize {
    fn parse(s: &Span) -> Result<(OpSize,Span),ParseError> {
        if let Some(t) = s.as_ident() {
            Ok((match t {
                "l" => OpSize::Long,
                "w" => OpSize::Word,
                "b" => OpSize::Byte,
                _ => return Err(ParseError::InvalidOpSize(s.clone()))
            }, s.clone()))
        } else { panic!("compiler broke: op size not an ident") }
    }
}

#[derive(Debug)]
pub enum Statement {
    Label {
        name: Span
    },
    Instruction {
        name: Span,
        size: Option<(OpSize,Span)>,
        arg: Argument   // TODO: parse this later?
    },
    RawData {
        data: Vec<u8>,
    },
    Attributes {
        attrs: Vec<Span>
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match self {
            Label { name } => write!(f, "Label {}", name),
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
        while let Some(c) = self.iter.next() {
            buf.push(c);
            match match &mut **buf {
                // Label:
                [ref mut name @ Ident(_), Symbol(':',_)] |
                [ref mut name @ AnonLabel(_)] =>
                    Some(Label { name: name.take() }),
                [Ident(ref c), ref rest.., LineBreak] if c.data == "db" => Some(RawData {
                    data: {
                        let mut dbuf = Vec::new();
                        let mut allow_comma = false;
                        // Allow any kind unless annotated as strict? TODO: local or global config based on attrs
                        for c in rest.iter() {
                            match c {
                                Whitespace => {},
                                Symbol(',',_) if allow_comma => allow_comma = false,
                                Byte(c) => { allow_comma = true; dbuf.write_u8(c.data); },
                                Word(c) => { allow_comma = true; dbuf.write_u16::<LittleEndian>(c.data); },
                                Long(c) => { allow_comma = true; dbuf.write_u24::<LittleEndian>(c.data); },
                                c => return Some(Err(ParseError::UnexpectedSymbol(c.clone())))
                            }
                        }
                        dbuf
                    }
                }),
                [ref mut name @ Ident(_), LineBreak] |
                [ref mut name @ Ident(_), Whitespace, Symbol(':',_)] => Some(Instruction {
                    name: name.take(),
                    size: None,
                    arg: Argument::parse(&[]).unwrap()
                }),
                [ref mut name @ Ident(_), Whitespace, ref rest.., LineBreak] |
                [ref mut name @ Ident(_), Whitespace, ref rest.., Whitespace, Symbol(':',_)] => Some(Instruction {
                    name: name.take(),
                    size: None,
                    arg: match Argument::parse(rest) {
                        Ok(c) => c,
                        Err(e) => return Some(Err(e))
                    }
                }),
                [ref mut name @ Ident(_), Symbol('.',_), ref size @ Ident(_), Whitespace, ref rest.., LineBreak] |
                [ref mut name @ Ident(_), Symbol('.',_), ref size @ Ident(_), Whitespace, ref rest.., Whitespace, Symbol(':',_)] => Some(Instruction {
                    name: name.take(),
                    size: Some(match OpSize::parse(size) {
                        Ok(c) => c,
                        Err(e) => return Some(Err(e))
                    }),   // todo: fix panic
                    arg: match Argument::parse(rest) {
                        Ok(c) => c,
                        Err(e) => return Some(Err(e))
                    }
                }),
                [Symbol('#',_), Symbol('[',_), ref rest.., Symbol(']',_)] => Some(Attributes {
                    attrs: rest.to_vec()
                }),
                [Whitespace] | [LineBreak] => None,
                [.., LineBreak] => return Some(Err(ParseError::GenericSyntaxError)),
                _ => continue
            } {
                Some(c) => return Some(Ok(c)),
                None => buf.clear()
            }
        }
        if buf.len() > 0 {
            return Some(Err(ParseError::GenericSyntaxError))
        }
        None
    }
}
