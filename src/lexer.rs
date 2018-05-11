use std::mem;
use std::ops;
use std::rc::Rc;
use std::fmt::{self, Display};

#[derive(Debug, Clone)]
pub struct Location {
    line: u32,
    column: u32,
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
            inner: LexerInner(LocationTracker { buf: r.next(), inner: r, loc: Location { line: 1, column: 1, file: Rc::new(filename) }}),
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


#[derive(Debug,Clone)]
pub enum SpanKind {
    Ident(String),      // TODO: SmallString?
    Symbol(char),
    Byte(u8),
    Word(u16),
    Long(u32),
    AnonLabel(i32),
    NumberError,        // Pseudotoken for malformed numbers
    Whitespace,         // For statement separation, compound operator parsing, etc.
    LineBreak
}

#[derive(Debug,Clone)]
pub struct Span {
    pub kind: SpanKind,
    pub start: Location,
    pub length: u32
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::SpanKind::*;
        match self.kind {
            Ident(ref s) => write!(f, "ident \"{}\"", s),
            Symbol(ref s) => write!(f, "symbol '{}'", s),
            Byte(ref s) => write!(f, "byte ${:02X}", s),
            Word(ref s) => write!(f, "word ${:04X}", s),
            Long(ref s) => write!(f, "long ${:06X}", s),
            AnonLabel(ref s) => write!(f, "anonymous label (depth {})", s),
            NumberError => write!(f, "malformed number"),
            Whitespace => write!(f, "whitespace"),
            LineBreak => write!(f, "line break")
        }?;
        write!(f, " at {}", self.start)
    }
}

impl Span {
    pub fn as_ident(&self) -> Option<&str> {
        if let SpanKind::Ident(ref s) = self.kind {
            Some(&s)
        } else {
            None
        }
    }
    fn ident(s: String, length: u32, start: Location) -> Self {
        Self { kind: SpanKind::Ident(s), start, length }
    }
    fn symbol(s: char, start: Location) -> Self {
        Self { kind: SpanKind::Symbol(s), start, length: 1 }
    }
    fn number(s: u32, length: u32, start: Location) -> Self {
        let kind = if length > 5 || s > 0xFFFF { SpanKind::Long(s) }
              else if length > 3 || s > 0x00FF { SpanKind::Word(s as u16) }
              else { SpanKind::Byte(s as u8) };
        Self { kind, start, length }
    }
    fn hex_number(s: u32, length: u32, start: Location) -> Self {
        // Span includes the $ so lengths are + 1
        let kind = if length > 5 { SpanKind::Long(s) }
              else if length > 3 { SpanKind::Word(s as u16) }
              else { SpanKind::Byte(s as u8) };
        Self { kind, start, length }
    }
    fn number_error(length: u32, start: Location) -> Self {
        Self { kind: SpanKind::NumberError, start, length }
    }
    fn whitespace(start: Location) -> Self {
        Self { kind: SpanKind::Whitespace, start, length: 0 }
    }
    fn line_break(start: Location) -> Self {
        Self { kind: SpanKind::LineBreak, start, length: 0 }
    }
    fn anon_label(amt: i32, length: u32, start: Location) -> Self {
        Self { kind: SpanKind::AnonLabel(amt), start, length: 0 }
    }
    pub fn is_whitespace(&self) -> bool {
        if let SpanKind::Whitespace | SpanKind::LineBreak = self.kind { true } else { false }
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
