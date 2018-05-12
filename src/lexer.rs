use std::mem;
use std::ops;
use std::rc::Rc;
use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub struct Location {
    line: u32,
    column: u32,
    byte: u32,
    file: Rc<String>
}

impl Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{} in {}", self.line, self.column, self.file)
    }
}

#[derive(Debug)]
struct LocationTracker<I> {
    inner: I,
    buf: Option<char>,
    loc: Location
}
impl<I> LocationTracker<I> {
    fn peek(&mut self) -> Option<char> {
        self.buf
    }
    fn location(&mut self) -> Location {
        self.loc.clone()
    }
    fn length(&self, other: &Location) -> u32 {
        if self.loc.line != other.line || self.loc.file != other.file {
            panic!("compiler machine broke");
        }
        self.loc.column - other.column
    }
}
impl<I: Iterator<Item=char>> Iterator for LocationTracker<I> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        let mut c = self.inner.next();
        mem::swap(&mut c, &mut self.buf);
        self.loc.byte += 1;
        match c {
            Some('\n') => {
                self.loc.column = 1;
                self.loc.line += 1;
            },
            Some(_) => self.loc.column += 1,
            _ => {}
        }
        c
    }
}

#[derive(Debug)]
pub struct Lexer<I> {
    inner: LexerInner<I>,
    finished: bool
}


impl<R: Iterator<Item=char>> Lexer<R> {
    pub fn new(filename: String, mut r: R) -> Self {
        Lexer {
            inner: LexerInner(LocationTracker { buf: r.next(), inner: r, loc: Location { line: 1, column: 1, byte: 0, file: Rc::new(filename) }}),
            finished: false
        }
    }
}

impl<R: Iterator<Item=char> + ::std::fmt::Debug> Iterator for Lexer<R> {
    type Item = Span;
    fn next(&mut self) -> Option<Span> {
        // Post-processing.
        if self.finished { return None; }
        match self.inner.next() {
            Some(c) => Some(c),
            None => {
                self.finished = true;
                Some(Span::line_break(self.inner.0.location()))
            }
        }
    }
}


#[derive(Debug)]
pub struct LexerInner<I>(LocationTracker<I>);

// TODO: invert the structure for easier matching

#[derive(Debug,Clone)]
pub struct SpanData<T> {
    pub data: T,
    pub start: Location,
    pub length: u32
}
impl<T: Display> Display for SpanData<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} at {}", self.data, self.start)
    }
}

impl<T: fmt::UpperHex> fmt::UpperHex for SpanData<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.data.fmt(f)?;
        write!(f, " at {}", self.start)
    }
}

#[derive(Debug,Clone)]
pub enum Span {
    Ident(SpanData<String>),
    Symbol(char, SpanData<char>),   // way easier matching
    Byte(SpanData<u8>),
    Word(SpanData<u16>),
    Long(SpanData<u32>),
    AnonLabel(SpanData<i32>),
    Successive(Vec<Span>),
    NumberError(SpanData<()>),
    Whitespace,
    LineBreak,
    Empty        // For the parser to use
}

/*
#[derive(Debug,Clone)]
pub enum SpanKind {
    Ident(String),      // TODO: SmallString?
    Symbol(char),
    Byte(u8),
    Word(u16),
    Long(u32),
    AnonLabel(i32),
    Successive(Vec<SpanKind>),
    NumberError,        // Pseudotoken for malformed numbers
    Whitespace,         // For statement separation, compound operator parsing, etc.
    LineBreak
}

#[derive(Debug,Clone)]
pub struct Span {
    pub kind: SpanKind,
    pub start: Location,
    pub length: u32
}*/

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Span::*;
        match self {
            Ident(ref s) => write!(f, "ident {}", s),
            Symbol(_, ref s) => write!(f, "symbol {}", s),
            Byte(ref s) => write!(f, "byte ${:02X}", s),
            Word(ref s) => write!(f, "word ${:04X}", s),
            Long(ref s) => write!(f, "long ${:06X}", s),
            AnonLabel(ref s) => write!(f, "anonymous label (depth {})", s),
            Successive(ref _s) => write!(f, "several spans"),   // TODO
            NumberError(_) => write!(f, "malformed number"),
            Whitespace => write!(f, "whitespace"),
            LineBreak => write!(f, "line break"),
            Empty => write!(f, "empty span"),
        }
    }
}

impl Span {
    pub fn as_ident(&self) -> Option<&str> {
        if let Span::Ident(ref s) = self {
            Some(&s.data)
        } else {
            None
        }
    }
    pub fn ident(data: String, length: u32, start: Location) -> Self {
        Span::Ident(SpanData { data, start, length })
    }
    pub fn symbol(data: char, start: Location) -> Self {
        Span::Symbol(data, SpanData { data, start, length: 1 })
    }
    pub fn number(data: u32, length: u32, start: Location) -> Self {
        if length > 5 || data > 0xFFFF {
            Span::Long(SpanData { data, start, length })
        } else if length > 3 || data > 0x00FF {
            Span::Word(SpanData { data: data as u16, start, length })
        } else {
            Span::Byte(SpanData { data: data as u8, start, length })
        }
    }
    pub fn hex_number(data: u32, length: u32, start: Location) -> Self {
        // Span includes the $ so lengths are + 1
        if length > 5 {
            Span::Long(SpanData { data, start, length })
        } else if length > 3 {
            Span::Word(SpanData { data: data as u16, start, length })
        } else {
            Span::Byte(SpanData { data: data as u8, start, length })
        }
    }
    pub fn number_error(length: u32, start: Location) -> Self {
        Span::NumberError(SpanData { data: (), start, length })
    }
    pub fn whitespace(start: Location) -> Self {
        Span::Whitespace
    }
    pub fn line_break(start: Location) -> Self {
        Span::LineBreak
    }
    pub fn anon_label(data: i32, length: u32, start: Location) -> Self {
        Span::AnonLabel(SpanData { data, start, length })
    }
    pub fn is_whitespace(&self) -> bool {
        if let Span::Whitespace | Span::LineBreak = self { true } else { false }
    }
}


impl<R: Iterator<Item=char> + ::std::fmt::Debug> Iterator for LexerInner<R> {
    type Item = Span;
    fn next(&mut self) -> Option<Span> {
        //println!("{:?}", self);
        let iter = &mut self.0;
        let start = iter.location();
        match iter.peek()? {
            'a' ... 'z' | 'A' ... 'Z' => { // ident
                let mut buf = String::new();
                while let 'a'...'z' | 'A' ... 'Z' | '0' ... '9' = iter.peek()? {
                    buf.push(iter.next()?);
                }
                Some(Span::ident(buf, iter.length(&start), start))
            },
            '0' ... '9' => { // dec number
                let mut buf = 0;
                while let Some('0' ... '9') = iter.peek() {
                    buf = buf * 10 + iter.next()?.to_digit(10)?;
                    // bounds check
                    if buf > 0xFFFFFF {
                        // skip to where the number ends
                        while let Some('0' ... '9') = iter.peek() { iter.next(); }
                        return Some(Span::number_error(iter.length(&start), start))
                    }
                }
                Some(Span::number(buf, iter.length(&start), start))
            },
            '$' => {        // hex number
                iter.next();
                let mut buf = 0;
                while let Some('0' ... '9') | Some('A' ... 'F') | Some('a' ... 'f') = iter.peek() {
                    buf = buf * 16 + iter.next()?.to_digit(16)?;
                    // bounds check
                    if buf > 0xFFFFFF {
                        // skip to where the number ends
                        while let Some('0' ... '9') = iter.peek() {}
                        return Some(Span::number_error(iter.length(&start), start))
                    }
                }
                Some(Span::hex_number(buf, iter.length(&start), start))
            },
            '+' => {
                let mut amt = 0;
                while let Some('+') = iter.peek() {
                    amt += 1;
                    iter.next();
                }
                Some(Span::anon_label(amt, iter.length(&start), start))
            },
            '-' => {
                let mut amt = 0;
                while let Some('-') = iter.peek() {
                    amt -= 1;
                    iter.next();
                }
                Some(Span::anon_label(amt, iter.length(&start), start))
            },
            '\n' | ' ' | '\t' | ';' => {
                let mut is_comment = false;
                let mut is_nl = false;
                loop {
                    match iter.peek()? {
                        '\n' if is_comment => {
                            is_comment = false;
                            is_nl = true;
                        },
                        _ if is_comment => {},
                        ' ' | '\t' => {},
                        '\n' => is_nl = true,
                        ';' => is_comment = true,
                        _ => return Some(if is_nl {
                            Span::line_break(start)
                        } else {
                            Span::whitespace(start)
                        })
                    }
                    iter.next();
                }
            },
            /*';' => {        // comment
                loop {
                    match iter.peek()? {
                        '\n' => return Some(Span::line_break(start)),
                        _ => { iter.next(); },
                    }
                }
            },
            ' ' | '\t' => {  // whitespace
                while let ' ' | '\t' = iter.peek()? { iter.next(); }
                Some(Span::whitespace(start))
            },
            '\n' => {
                while let '\n' = iter.peek()? { iter.next(); }
                Some(Span::line_break(start))
            },*/
            // other stuff
            _ => Some(Span::symbol(iter.next()?, start))
        }
    }
}
