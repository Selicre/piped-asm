use std::fmt;
use std::io::{self,prelude::*,BufReader};
use std::iter::Peekable;

use lexer::{Span,SpanData};
use instructions::SizeHint;
use expression::{Expression,ExprNode,ExprError};
use compiler::CompilerState;
use byteorder::{WriteBytesExt,LittleEndian};
use attributes::{Attribute,AttributeError};
use n_peek::NPeekable;

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
        let (mut kind, expr_spans) = modeparser! [ spans => [
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
        // Additional check for a malformed mode
        if expr_spans.len() >= 3 && [x_suf,y_suf,s_suf,parens,brackets].into_iter().any(|i| i(expr_spans).is_some()) {
            return Err(ParseError::UnknownAddressingMode(Span::coagulate(spans)))
        }
        let spans = expr_spans;
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
    Define {
        label: Span,
        expr: Expression
    },
    Nothing,
    Error(ParseError)
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match self {
            Label { name, .. } => write!(f, "Label {}", name),
            LocalLabel { name, .. } => write!(f, "Local label {}", name),
            Instruction { name, arg, .. } => write!(f, "Instruction: {} {}", name, arg),
            RawData { data, .. } => {
                write!(f, "Raw data: ")?;
                for i in data { write!(f, "{:02X} ", i)? }
                Ok(())
            },
            c => write!(f, "{:?}", c)
        }
    }
}


#[derive(Debug)]
pub enum ParseError {
    ExprError(ExprError),
    MalformedHexString(Span),
    UnknownCommand(Vec<Span>),
    InvalidOpSize(Span),
    GenericSyntaxError,
    UnknownAddressingMode(Span),
    IO(io::Error),
    UnexpectedSymbol(Span),
    AttributeError(AttributeError),
    UnexpectedEOF(String),
}

// obviously TODO
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use colors::prelude::*;
        write!(f, "{}", "Error: ".pretty() + Color(1) + Bold)?;
        match self {
            ParseError::ExprError(ref e) => {
                write!(f, "Expression error {:?}", e)
            },
            ParseError::UnknownAddressingMode(ref e) => {
                write!(f, "Unknown addressing mode {} at {{location}}", e)
            },
            e => write!(f, "Other error (TODO!)\n{:?}", e)
        }
    }
}
use std::option::NoneError;
impl From<NoneError> for ParseError {
    fn from(f: NoneError) -> ParseError {
        ParseError::UnexpectedEOF(String::new())
    }
}
pub struct Parser<S: Iterator> {
    iter: NPeekable<S>,
    incsrc: Option<Box<Iterator<Item=Statement>>>,
    state: CompilerState,
}
impl<S: Iterator<Item=Span>> Parser<S> {
    pub fn new(iter: S, state: CompilerState) -> Self {
        Self { iter: NPeekable::new(iter), state, incsrc: None }
    }
    // .next() but skip whitespace
    fn skip_wsp(&mut self) -> Option<Span> {
        self.iter.by_ref().filter(|c| !c.is_whitespace()).next()
    }
    fn define(&mut self) -> Result<Statement, ParseError> {
        use self::Span::*;
        let label = self.iter.next()?;
        if !label.as_ident().is_some() { return Err(ParseError::GenericSyntaxError) }
        let buf = self.iter.by_ref()
            .take_while(|c| c != &LineBreak && !c.is_symbol(':'))
            .filter(|c| c != &Whitespace)
            .collect::<Vec<_>>();
        Ok(Statement::Define {
            label,
            expr: Expression::parse(&buf, &mut self.state.borrow_mut().lls).map_err(ParseError::ExprError)?
        })
    }
    fn incsrc(&mut self, attrs: &Vec<Attribute>) -> Result<Option<Statement>,ParseError> {
        use self::Span::*;
        use self::Statement::*;
        use std::fs::File;
        use lexer::Lexer;
        let state = self.state.clone();
        let filename = self.skip_wsp()?.as_string().ok_or(ParseError::GenericSyntaxError)?;
        let file = File::open(&filename).map_err(ParseError::IO)?;
        let file = BufReader::new(file);
        let chars = file.chars().map(|c| c.unwrap());
        let lexed = Box::new(Lexer::new(filename.to_string(), chars)) as Box<Iterator<Item=_>>;
        let mut parsed = Box::new(Parser::new(lexed, state));
        let first_stmt = parsed.next();
        self.incsrc = Some(parsed);
        Ok(first_stmt)
    }
    fn incbin(&mut self) -> Result<Statement,ParseError> {
        use std::fs::File;
        use std::io::Read;
        let filename = self.skip_wsp()?.as_string().ok_or(ParseError::GenericSyntaxError)?;
        let mut data = Vec::new();
        File::open(&filename).map_err(ParseError::IO)?.read_to_end(&mut data).map_err(ParseError::IO)?;
        Ok(Statement::RawData {
            data,
            pending_exprs: vec![]
        })
    }
    fn inline_data(&mut self, attrs: Vec<Attribute>, size: SizeHint) -> Result<Statement,ParseError> {
        use self::Span::*;
        use self::Statement::*;
        let iter = &mut self.iter;
        // todo: remove this
        let buf = iter.by_ref()
            .take_while(|c| c != &LineBreak && !c.is_symbol(':'))
            .filter(|c| c != &Whitespace)
            .collect::<Vec<_>>();
        let mut dbuf = Vec::with_capacity(16);
        let mut pending_exprs = Vec::new();
        for i in buf.split(|c| c.is_symbol(',')) {
            let mut expr = Expression::parse(&i, &mut self.state.borrow_mut().lls).map_err(ParseError::ExprError)?;
            // db "thing"
            if let ExprNode::Str(s) = expr.root {
                dbuf.write(s.as_bytes()).unwrap();
                continue
            }
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
        Ok(RawData {
            data: dbuf,
            pending_exprs
        })
    }
    fn inline_hex_data(&mut self, attrs: Vec<Attribute>) -> Result<Statement,ParseError> {
        use self::Span::*;
        use self::Statement::*;
        let data = self.skip_wsp()?;
        let d = &if let String(ref d) = data { d } else { return Err(ParseError::GenericSyntaxError) }.data;
        if d.len() % 2 != 0 { return Err(ParseError::MalformedHexString(data)); }
        let buf = d.as_bytes().chunks(2).map(|c| {
            Some(((c[1] as char).to_digit(16)?
                + (c[0] as char).to_digit(16)? * 16) as u8)
        }).collect::<Option<_>>()
            .ok_or(ParseError::MalformedHexString(data.clone()))?;
        Ok(RawData {
            data: buf,
            pending_exprs: vec![]
        })
    }
    fn instruction(&mut self, attrs: Vec<Attribute>, id1: SpanData<String>) -> Result<Statement,ParseError> {
        use self::Span::*;
        use self::Statement::*;
        let iter = &mut self.iter;
        let mut size_hint = None;
        match iter.peek(0) {
            // add a ".w" size hint
            Some(Symbol('.',_)) => {
                iter.next()?;
                let size = iter.next()?;
                let size_p = size.as_ident()
                    .and_then(SizeHint::parse)
                    .ok_or(ParseError::InvalidOpSize(size.clone()))?;
                size_hint = Some((size_p, Some(size)))
            },
            _ => {}
        }
        // Note: in the future, there will be no `.collect()`
        let buf = iter.by_ref()
            .take_while(|c| c != &LineBreak && !c.is_symbol(':'))
            .filter(|c| c != &Whitespace)
            .collect::<Vec<_>>();
        let arg = if buf.len() > 0 {
            Argument::parse(&buf, &self.state)?
        } else {
            Argument::implied()
        };
        let c = Instruction {
            name: Ident(id1), attrs, size: size_hint.unwrap_or_default(), arg
        };
        Ok(c)
    }
}



impl<S: Iterator<Item=Span>> Iterator for Parser<S> {
    type Item = Statement;
    fn next(&mut self) -> Option<Statement> {
        if let Some(mut c) = self.incsrc.take() {
            match c.next() {
                Some(d) => { self.incsrc = Some(c); return Some(d) },
                None => { self.incsrc = None }
            }
        }
        use self::Span::*;
        use self::Statement::*;
        let mut attrs = Vec::new();
        let res = (|| {
            // loop allows the use of "continue"
            loop {
                let first = match self.skip_wsp() {
                    Some(c) => c,
                    None => return Ok(None)
                };
                return Ok(Some(match first {
                Ident(id1) => match self.iter.peek(0)? {
                    Symbol(':',_) => {
                        self.iter.next();
                        Label {
                            attrs, name: Ident(id1)
                        }
                    },
                    _ => match &*id1.data {
                        "define" => self.define()?,
                        "incsrc" => match self.incsrc(&attrs)? {
                            Some(c) => c,
                            None => continue
                        },
                        "incbin" => self.incbin()?,
                        "db" => self.inline_data(attrs, SizeHint::Byte)?,
                        "dw" => self.inline_data(attrs, SizeHint::Word)?,
                        "dl" => self.inline_data(attrs, SizeHint::Long)?,
                        "dbx" => self.inline_hex_data(attrs)?,
                        _ => self.instruction(attrs, id1)?
                    }
                },
                Symbol('.',_) => {
                    let iter = &mut self.iter;
                    let mut depth = 0;
                    while iter.peek(0)?.is_symbol('.') { depth += 1; iter.next(); }
                    let name = iter.next()?;
                    // skip ending ':'
                    if iter.peek(0).map(|c| c.is_symbol(':')).unwrap_or(false) { iter.next(); }
                    LocalLabel { depth, name }
                },
                name @ PosLabel(_) | name @ NegLabel(_) => {
                    let iter = &mut self.iter;
                    // Allow for +: and -:, same as asar
                    if iter.peek(0).map(|c| c.is_symbol(':')).unwrap_or(false) { iter.next(); }
                    Label {
                        attrs, name
                    }
                },
                Symbol('#',_) => {
                    let iter = &mut self.iter;
                    if !iter.next()?.is_symbol('[') { println!("["); return Err(ParseError::GenericSyntaxError) }
                    let buf = iter.by_ref()
                        .take_while(|c| !c.is_symbol(']'))
                        .collect::<Vec<_>>();
                    let c = buf.split(|c| c.is_symbol(','))
                        .map(Attribute::from_span)
                        .collect::<Result<Vec<_>,_>>()
                        .map_err(ParseError::AttributeError)?;
                    attrs.extend(c);
                    continue;
                },
                c => { println!("c: {:?}", c); Nothing }
            })) }
        })();
        match res {
            Ok(Some(c)) => Some(c),
            Ok(None) => None,
            Err(e) => Some(Statement::Error(e))
        }
    }
}
