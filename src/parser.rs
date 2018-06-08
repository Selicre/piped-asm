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

use attributes::{Attribute,AttributeError};

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
        attrs: Vec<Attribute>,
        name: Span
    },
    LocalLabel {
        depth: usize,
        name: Span
    },
    Instruction {
        attrs: Vec<Attribute>,
        name: Span,
        size: (SizeHint,Option<Span>),
        arg: Argument
    },

    RawData {
        data: Vec<u8>,
        pending_exprs: Vec<(usize,Expression)>
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
            Label { name, .. } => write!(f, "Label {}", name),
            LocalLabel { depth, name } => write!(f, "Local label {}", name),
            Instruction { name, size, arg, .. } => write!(f, "Instruction: {} {}", name, arg),
            Attributes { attrs } => write!(f, "Attributes"),
            RawData { data, .. } => {
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
    MalformedHexString(Span),
    UnknownCommand(Vec<Span>),
    InvalidOpSize(Span),
    GenericSyntaxError,
    UnknownAddressingMode(Span),
    IO(io::Error),
    UnexpectedSymbol(Span),
    AttributeError(AttributeError)
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
    // todo: do this properly
    incsrc: Option<Box<Iterator<Item=Result<Statement,ParseError>>>>,
    state: CompilerState,
    buf: Vec<Span>
}
impl<S: Iterator<Item=Span>> Parser<S> {
    pub fn new(iter: S, state: CompilerState) -> Self {
        Self { iter, state, incsrc: None, buf: Default::default() }
    }
}
impl<S: Iterator<Item=Span>> Iterator for Parser<S> {
    type Item = Result<Statement,ParseError>;
    fn next(&mut self) -> Option<Result<Statement,ParseError>> {
        if let Some(mut c) = self.incsrc.take() {
            match c.next() {
                Some(d) => { self.incsrc = Some(c); return Some(d) },
                None => { self.incsrc = None }
            }
        }
        use self::Span::*;
        use self::Statement::*;
        let mut buf = QueueClearHandle(&mut self.buf);
        let mut attrs = Vec::new();
        let iter = &mut self.iter;
        let state = &self.state;
        let incsrc = &mut self.incsrc;
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
            // TODO: this entire thing should be replaced with a proper parser tree.
            return Ok(Some(match &mut **buf {
                // Label:
                [ref mut name @ Ident(_), Symbol(':',_)] |
                [ref mut name @ PosLabel(_)] | [ref mut name @ NegLabel(_)] =>
                    Label { attrs, name: name.take() },
                // closure below is a workaround
                [ref mut dots.., ref mut name @ Ident(_), Whitespace] |
                [ref mut dots.., ref mut name @ Ident(_), LineBreak] |
                [ref mut dots.., ref mut name @ Ident(_), Symbol(':',_)]
                    if (|| dots.len() > 0 && dots.iter().all(|c| if let Symbol('.',_) = c { true } else { false }))() =>
                        LocalLabel { depth: dots.len() - 1, name: name.take() },
                [Ident(ref mut c), Whitespace, Span::String(ref mut filename), LineBreak] if c.data == "incsrc" => {
                    use std::io::{self,prelude::*,BufReader};
                    use std::fs::File;
                    use lexer::Lexer;
                    let state = state.clone();
                    println!("loading {}", filename);
                    let file = File::open(&filename.data).map_err(ParseError::IO)?;
                    let file = BufReader::new(file);
                    // no idea why a box is needed here
                    let chars = file.chars().map(|c| c.unwrap());
                    let lexed = Box::new(Lexer::new(filename.data.clone(), chars)) as Box<Iterator<Item=_>>;
                    let mut parsed = Box::new(Parser::new(lexed, state));
                    let first_stmt = parsed.next();
                    *incsrc = Some(parsed);
                    match first_stmt {
                        Some(c) => c?,
                        None => { clear = true; continue }
                    }
                },
                [Ident(ref c), Whitespace, Span::String(ref name), LineBreak] if c.data == "incbin" => RawData {
                    data: {
                        use std::fs::File;
                        use std::io::Read;
                        let mut c = Vec::new();
                        println!("loading {}", name);
                        File::open(&name.data).map_err(ParseError::IO)?.read_to_end(&mut c).map_err(ParseError::IO)?;
                        c
                    },
                    pending_exprs: vec![]
                },
                [Ident(ref c), Whitespace, String(ref s), LineBreak] if c.data == "ds" => RawData {
                    data: s.data.as_bytes().to_vec(),
                    pending_exprs: vec![]
                },
                [Ident(ref c), Whitespace, String(ref s), LineBreak] if c.data == "dbx" => RawData {
                    data: {
                        if s.data.len() % 2 != 0 { return Err(ParseError::MalformedHexString(String(s.clone()))); }
                        s.data.as_bytes().chunks(2).map(|c| {
                            Some(((c[1] as char).to_digit(16)? + (c[0] as char).to_digit(16)? * 16) as u8)
                        }).collect::<Option<_>>().ok_or(ParseError::MalformedHexString(String(s.clone())))?
                    },
                    pending_exprs: vec![]
                },
                [Ident(ref c), ref rest.., LineBreak] if c.data == "db" || c.data == "dw" || c.data == "dl" => {
                    let size = match &*c.data {
                        "db" => SizeHint::Byte,
                        "dw" => SizeHint::Word,
                        "dl" => SizeHint::Long,
                        _ => unreachable!()
                    };
                    let mut dbuf = Vec::new();
                    let mut allow_comma = false;
                    let mut pending_exprs = Vec::new();
                    for i in rest.split(|c| c.is_symbol(',')) {
                        let mut expr = Expression::parse(&i, &mut state.borrow_mut().lls).map_err(ParseError::ExprError)?;
                        let data = if let Some(c) = expr.is_const() {
                            c
                        } else {
                            expr.with_size(size);
                            pending_exprs.push((dbuf.len(), expr));
                            0
                        };
                        match size {
                            SizeHint::Byte => dbuf.write_u8(data as u8).unwrap(),
                            SizeHint::Word => dbuf.write_u16::<LittleEndian>(data as u16).unwrap(),
                            SizeHint::Long => dbuf.write_u24::<LittleEndian>(data as u32).unwrap(),
                            _ => unreachable!()
                        }
                    }
                    RawData {
                        data: dbuf,
                        pending_exprs
                    }
                },
                [ref mut name @ Ident(_), LineBreak] |
                [ref mut name @ Ident(_), Whitespace, Symbol(':',_)] => Instruction {
                    attrs,
                    name: name.take(),
                    size: Default::default(),
                    arg: Argument::implied()
                },
                [ref mut name @ Ident(_), Whitespace, ref mut rest.., LineBreak] |
                [ref mut name @ Ident(_), Whitespace, ref mut rest.., Whitespace, Symbol(':',_)] => Instruction {
                    attrs,
                    name: name.take(),
                    size: Default::default(),
                    arg: Argument::parse(rest, state)?
                },
                [ref mut name @ Ident(_), Symbol('.',_), ref mut size @ Ident(_), Whitespace, ref mut rest.., LineBreak] |
                [ref mut name @ Ident(_), Symbol('.',_), ref mut size @ Ident(_), Whitespace, ref mut rest.., Whitespace, Symbol(':',_)]
                    => Instruction {
                    attrs,
                    name: name.take(),
                    size: (SizeHint::parse(size.as_ident().unwrap()).ok_or(ParseError::GenericSyntaxError)?, Some(size.take())),
                    arg: Argument::parse(rest, state)?
                },
                [Symbol('#',_), Symbol('[',_), ref rest.., Symbol(']',_)] => {
                    let c = rest.split(|c| c.is_symbol(','))
                        .map(Attribute::from_span)
                        .collect::<Result<Vec<_>,_>>()
                        .map_err(ParseError::AttributeError)?;
                    attrs.extend(c);
                    clear = true;
                    continue;
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
