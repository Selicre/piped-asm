use std::collections::VecDeque;
use std::ops;
use std::cell::Cell;
use std::fmt;
use std::io::{self,Write};

use lexer::{Span,SpanData};

use instructions::SizeHint;

use expression::{Expression,ExprNode,ExprError};

use compiler::CompilerState;

use byteorder::{WriteBytesExt,LittleEndian};

#[derive(Debug, PartialEq)]
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
    pub expr: Expression
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} ({})", self.kind, self.expr)
    }
}



impl Argument {
    fn implied() -> Self {
        Argument { kind: ArgumentKind::Implied, expr: Expression::empty() }
    }
    fn parse(spans: &[Span], state: &CompilerState) -> Result<Self, ParseError> {
        use self::Span::*;
        use self::ArgumentKind::*;
        // todo: put into a macro, this is seriously stupid
        fn x_suf(sp: &[Span]) -> Option<&[Span]> {    // ,x
            let len = sp.len();
            let (rest,s) = sp.split_at(len-2);
            if (s[1].is_ident("x") || s[1].is_ident("X")) && s[0].is_symbol(',') {
                Some(rest)
            } else { None }
        }
        fn y_suf(sp: &[Span]) -> Option<&[Span]> {    // ,y
            let len = sp.len();
            let (rest,s) = sp.split_at(len-2);
            if (s[1].is_ident("y") || s[1].is_ident("Y")) && s[0].is_symbol(',') {
                Some(rest)
            } else { None }
        }
        fn s_suf(sp: &[Span]) -> Option<&[Span]> {    // ,s
            let len = sp.len();
            let (rest,s) = sp.split_at(len-2);
            if (s[1].is_ident("s") || s[1].is_ident("S")) && s[0].is_symbol(',') {
                Some(rest)
            } else { None }
        };
        fn parens(sp: &[Span]) -> Option<&[Span]> {
            let len = sp.len();
            if sp[0].is_symbol('(') && sp[len-1].is_symbol(')') {
                Some(&sp[1..len-1])
            } else { None }
        }
        fn brackets(sp: &[Span]) -> Option<&[Span]> {
            let len = sp.len();
            if sp[0].is_symbol('[') && sp[len-1].is_symbol(']') {
                Some(&sp[1..len-1])
            } else { None }
        }

        macro_rules! modeparser {
            ($spans:ident => [$default:expr, $($fn:ident => $t:tt),*]) => {{
                if $spans.len() < 3 {
                    ($default, $spans)
                } $( else if let Some(c) = $fn($spans) {
                    modeparser!(c => $t)
                } )* else {
                    ($default, $spans)
                }
            }};
            ($spans:ident => $expr:expr) => { ($expr, $spans) };
        }
        let (mut kind, spans) = modeparser! [ spans => [
            Direct,
            x_suf => IndexedX,
            y_suf => [
                IndexedY,
                parens => [
                    IndY,
                    s_suf => StackY
                ],
                brackets => IndLongY
            ],
            s_suf => Stack,
            parens => [
                Indirect,
                x_suf => IndX
            ],
            brackets => IndLong
        ]];
        // TODO: full support for expressions
        match spans {
            [Symbol('#',_), ref c1, Symbol(',',_), Symbol('#',_), ref c2] |
            [ref c1, Symbol(',',_), ref c2] => {
                return Ok(Argument { kind: TwoArgs(c1.clone(), c2.clone()), expr: Expression::empty() });
            },
            _ => {}
        }
        let expr = if kind == Direct && spans.len() >= 2 && spans[0].is_symbol('#') {
            kind = Constant;
            Expression::parse(&spans[1..], &mut state.borrow_mut().lls).map_err(ParseError::ExprError)?
        } else {
            Expression::parse(spans, &mut state.borrow_mut().lls).map_err(ParseError::ExprError)?
        };
        Ok(Argument { kind, expr })
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
    },
    Error(ParseError)
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match self {
            Label { name } => write!(f, "Label {}", name),
            LocalLabel { depth, name } => write!(f, "Local label {}", name),
            Instruction { name, size, arg } => write!(f, "Instruction: {} {}", name, arg),
            Attributes { attrs } => write!(f, "Attributes"),
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
    ExprError(ExprError),
    UnknownCommand(Vec<Span>),
    InvalidOpSize(Span),
    GenericSyntaxError,
    UnknownAddressingMode(Span),
    IO(io::Error),
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
    state: CompilerState,
    buf: Vec<Span>
}
impl<S: Iterator<Item=Span>> Parser<S> {
    pub fn new(iter: S, state: CompilerState) -> Self {
        Self { iter, state, buf: Default::default() }
    }
}
impl<S: Iterator<Item=Span>> Iterator for Parser<S> {
    type Item = Result<Statement,ParseError>;
    fn next(&mut self) -> Option<Result<Statement,ParseError>> {
        use self::Span::*;
        use self::Statement::*;
        let mut buf = QueueClearHandle(&mut self.buf);
        let iter = &mut self.iter;
        let state = &self.state;
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
                [ref mut dots.., ref mut name @ Ident(_), Whitespace] |
                [ref mut dots.., ref mut name @ Ident(_), LineBreak] |
                [ref mut dots.., ref mut name @ Ident(_), Symbol(':',_)]
                    if (|| dots.len() > 0 && dots.iter().all(|c| if let Symbol('.',_) = c { true } else { false }))() =>
                        LocalLabel { depth: dots.len() - 1, name: name.take() },
                [Ident(ref c), Whitespace, Span::String(ref name), LineBreak] if c.data == "incbin" => RawData {
                    data: {
                        use std::fs::File;
                        use std::io::Read;
                        let mut c = Vec::new();
                        File::open(&name.data).map_err(ParseError::IO)?.read_to_end(&mut c).map_err(ParseError::IO)?;
                        c
                    }
                },
                [Ident(ref c), ref rest.., LineBreak] if c.data == "db" || c.data == "dw" || c.data == "dl" => RawData {
                    data: {
                        let mut dbuf = Vec::new();
                        let mut allow_comma = false;
                        // Allow any kind unless annotated as strict? TODO: local or global config based on attrs
                        // todo: expr parsing
                        for c in rest.iter() {
                            match c {
                                Whitespace => {},
                                Symbol(',',_) if allow_comma => allow_comma = false,
                                Byte(c) => { allow_comma = true; dbuf.write_u8(c.data as u8).unwrap(); },
                                Word(c) => { allow_comma = true; dbuf.write_u16::<LittleEndian>(c.data as u16).unwrap(); },
                                Long(c) => { allow_comma = true; dbuf.write_u24::<LittleEndian>(c.data as u32).unwrap(); },
                                // todo: make this work based on db/dw/dl
                                Number(c) => { allow_comma = true; dbuf.write_u8(c.data as u8).unwrap(); },
                                String(c) => { allow_comma = true; dbuf.write(c.data.as_bytes()).unwrap(); },
                                Ident(c) => { /* TODO: labels */ },
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
                    arg: Argument::implied()
                },
                [ref mut name @ Ident(_), Whitespace, ref mut rest.., LineBreak] |
                [ref mut name @ Ident(_), Whitespace, ref mut rest.., Whitespace, Symbol(':',_)] => Instruction {
                    name: name.take(),
                    size: Default::default(),
                    arg: Argument::parse(rest, state)?
                },
                [ref mut name @ Ident(_), Symbol('.',_), ref mut size @ Ident(_), Whitespace, ref mut rest.., LineBreak] |
                [ref mut name @ Ident(_), Symbol('.',_), ref mut size @ Ident(_), Whitespace, ref mut rest.., Whitespace, Symbol(':',_)]
                    => Instruction {
                    name: name.take(),
                    size: (SizeHint::parse(size.as_ident().unwrap()).ok_or(ParseError::GenericSyntaxError)?, Some(size.take())),
                    arg: Argument::parse(rest, state)?
                },
                [Symbol('#',_), Symbol('[',_), ref rest.., Symbol(']',_)] => Attributes {
                    attrs: rest.to_vec()
                },
                [Whitespace] | [LineBreak] => { clear = true; continue; },
                [ref line.., LineBreak] => return Err(ParseError::UnknownCommand(line.to_vec())),
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
