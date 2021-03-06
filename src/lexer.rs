use std::mem;
use std::rc::Rc;
use std::fmt::{self, Display};
use std::io;

use instructions::SizeHint;

#[derive(Debug, Clone, Default, PartialEq)]
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

pub fn from_filename(filename: String) -> Result<Lexer<impl Iterator<Item=char>>, io::Error> {
    use std::fs::File;
    use std::io::Read;
    let mut buf = String::new();
    File::open(filename.clone())?.read_to_string(&mut buf)?;
    Ok(Lexer::new(filename, buf.chars().collect::<Vec<_>>().into_iter()))
}

impl<R: Iterator<Item=char>> Iterator for Lexer<R> {
    type Item = Span;
    fn next(&mut self) -> Option<Span> {
        // Post-processing.
        if self.finished { return None; }
        match self.inner.next() {
            Some(c) => Some(c),
            None => {
                self.finished = true;
                Some(Span::line_break())
            }
        }
    }
}


pub struct LexerInner<I>(LocationTracker<I>);

// TODO: invert the structure for easier matching

#[derive(Debug,Clone,PartialEq)]
pub struct SpanData<T> {
    pub data: T,
    pub start: Location,
    pub length: u32
}
impl<T: Display> Display for SpanData<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl<T: fmt::UpperHex> fmt::UpperHex for SpanData<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.data.fmt(f)
    }
}

impl<T> SpanData<T> {
    pub fn create(data: T) -> Self {
        SpanData { data, start: Default::default(), length: 0 }
    }
}

#[derive(Debug,Clone,PartialEq)]
pub enum Span {
    Ident(SpanData<String>),
    Symbol(char, SpanData<char>),   // way easier matching
    Number(SpanData<i32>),
    Byte(SpanData<i32>),
    Word(SpanData<i32>),
    Long(SpanData<i32>),
    PosLabel(SpanData<usize>),
    NegLabel(SpanData<usize>),
    String(SpanData<String>),
    Successive(Vec<Span>),
    NumberError(SpanData<()>),
    Whitespace,
    LineBreak,
    Empty
}


impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Span::*;
        match self {
            Ident(ref s) => write!(f, "{}", s),
            Symbol(_, ref s) => write!(f, "{}", s),
            Number(ref s) => write!(f, "{}", s),
            Byte(ref s) => write!(f, "${:02X}", s),
            Word(ref s) => write!(f, "${:04X}", s),
            Long(ref s) => write!(f, "${:06X}", s),
            PosLabel(ref s) => write!(f, "+({})", s),
            NegLabel(ref s) => write!(f, "-({})", s),
            String(ref s) => write!(f, "\"{}\"", s),
            Successive(ref s) => { for i in s { write!(f, "{}", i)? } Ok(()) },
            NumberError(_) => write!(f, "malformed number"),
            Whitespace => write!(f, " "),
            LineBreak => write!(f, "\\n"),
            Empty => write!(f, "<empty>"),
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
    pub fn as_number(&self) -> Option<i32> {
        use self::Span::*;
        match self {
            Number(s) | Byte(s) | Word(s) | Long(s) => Some(s.data),
            _ => None
        }
    }
    pub fn as_string(self) -> Option<String> {
        if let Span::String(s) = self {
            Some(s.data)
        } else {
            None
        }
    }
    pub fn take(&mut self) -> Self {
        let mut out = Span::Empty;
        ::std::mem::swap(self, &mut out);
        out
    }
    pub fn is_local_label(&self) -> Option<(usize, &Span)> {
        if let Span::Successive(c) = self {
            if c.len() > 1 {
                if let Some(span @ Span::Ident(_)) = c.get(c.len()-1) {
//              if let [ref dots.., ref span @ Span::Ident(_)] = &**c {
                    let dots = &c[..c.len()-1];
                    if dots.iter().all(|c| if let Span::Symbol('.',_) = c { true } else { false }) {
                        return Some((dots.len() - 1, span))
                    }
                }
            }
        }
        None
    }
    pub fn size(&self) -> SizeHint {
        use self::Span::*;
        match self {
            Byte(_) => SizeHint::Byte,
            Word(_) => SizeHint::Word,
            Long(_) => SizeHint::Long,
            Number(_) => SizeHint::Byte,
            _ => SizeHint::Unspecified
        }
    }
    pub fn coagulate(entries: &[Span]) -> Span {
        if entries.len() == 0 { Span::Empty }
        else if entries.len() == 1 { entries[0].clone() }
        else { Span::Successive(entries.iter().cloned().collect()) }
    }
    pub fn is_whitespace(&self) -> bool {
        if let Span::Whitespace | Span::LineBreak = self { true } else { false }
    }
    pub fn is_symbol(&self, s: char) -> bool {
        if let Span::Symbol(c,_) = self { *c == s } else { false }
    }
    pub fn is_ident(&self, s: &str) -> bool {
        if let Span::Ident(c) = self { c.data == s } else { false }
    }
    pub fn ident(data: String, length: u32, start: Location) -> Self {
        Span::Ident(SpanData { data, start, length })
    }
    pub fn symbol(data: char, start: Location) -> Self {
        Span::Symbol(data, SpanData { data, start, length: 1 })
    }
    pub fn number(data: i32, length: u32, start: Location) -> Self {
        Span::Number(SpanData { data, start, length })
    }
    pub fn hex_number(data: i32, length: u32, start: Location) -> Self {
        // Span includes the $ so lengths are + 1
        let f = if length > 5 { Span::Long }
        else if length > 3 { Span::Word }
        else { Span::Byte };
        f(SpanData { data, start, length })
    }
    pub fn number_error(length: u32, start: Location) -> Self {
        Span::NumberError(SpanData { data: (), start, length })
    }
    pub fn whitespace() -> Self {
        Span::Whitespace
    }
    pub fn line_break() -> Self {
        Span::LineBreak
    }
    pub fn pos_label(data: usize, length: u32, start: Location) -> Self {
        Span::PosLabel(SpanData { data, start, length })
    }
    pub fn neg_label(data: usize, length: u32, start: Location) -> Self {
        Span::NegLabel(SpanData { data, start, length })
    }
    pub fn character(data: i32, length: u32, start: Location) -> Self {
        Span::Number(SpanData { data, start, length })
    }
    pub fn string(data: String, length: u32, start: Location) -> Self {
        Span::String(SpanData { data, start, length })
    }
}


impl<R: Iterator<Item=char>> Iterator for LexerInner<R> {
    type Item = Span;
    fn next(&mut self) -> Option<Span> {
        let iter = &mut self.0;
        let start = iter.location();
        match iter.peek()? {
            'a' ... 'z' | 'A' ... 'Z' | '_' => { // ident
                let mut buf = String::new();
                while let Some('a'...'z') | Some('A' ... 'Z') | Some('0' ... '9') | Some('_') = iter.peek() {
                    buf.push(iter.next()?);
                }
                Some(Span::ident(buf, iter.length(&start), start))
            },
            '0' ... '9' => { // dec number
                let mut buf = 0;
                while let Some('0' ... '9') = iter.peek() {
                    buf = buf * 10 + iter.next()?.to_digit(10)? as i32;
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
                if let Some('0'...'9') | Some('A' ... 'F') | Some('a' ... 'f') = iter.peek() {} else {
                    // if there is no number after this, it's a regular symbol (for variables, etc.)
                    return Some(Span::symbol('$', start));
                }
                let mut buf = 0;
                while let Some('0' ... '9') | Some('A' ... 'F') | Some('a' ... 'f') = iter.peek() {
                    buf = buf * 16 + iter.next()?.to_digit(16)? as i32;
                    // bounds check
                    if buf > 0xFFFFFF {
                        // skip to where the number ends
                        while let Some('0' ... '9') = iter.peek() {}
                        return Some(Span::number_error(iter.length(&start), start))
                    }
                }
                Some(Span::hex_number(buf, iter.length(&start), start))
            },
            '%' => {        // binary number
                iter.next();
                let mut buf = 0;
                while let Some('0' ... '1') = iter.peek() {
                    buf = buf * 2 + iter.next()?.to_digit(2)? as i32;
                    // bounds check
                    if buf > 0xFFFFFF {
                        // skip to where the number ends
                        while let Some('0' ... '1') = iter.peek() {}
                        return Some(Span::number_error(iter.length(&start), start))
                    }
                }
                Some(Span::number(buf, iter.length(&start), start))
            },
            '+' => {
                let mut amt = 0;
                while let Some('+') = iter.peek() {
                    amt += 1;
                    iter.next();
                }
                Some(Span::pos_label(amt-1, iter.length(&start), start))
            },
            '-' => {
                let mut amt = 0;
                while let Some('-') = iter.peek() {
                    amt += 1;
                    iter.next();
                }
                Some(Span::neg_label(amt-1, iter.length(&start), start))
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
                            Span::line_break()
                        } else {
                            Span::whitespace()
                        })
                    }
                    iter.next();
                }
            },
            // ascii char
            '\'' => {
                let _ = iter.next()?;
                let ch = match iter.next()? {
                    '\\' => match iter.next()? {
                        '\'' => '\'',
                        '\\' => '\\',
                        '"' => '"',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        _ => panic!("invalid escape")
                    },
                    '\n' => panic!("Lexer error (todo: handle)"),
                    c => c
                } as u32;
                if iter.next()? != '\'' { panic!("char too long"); }
                if ch > 0x7F {
                    panic!("Can't handle unicode chars yet");
                }
                Some(Span::character(ch as i32, iter.length(&start), start))
            },
            // ascii string
            '\"' => {
                let _ = iter.next()?;
                let mut buf = String::new();
                loop {
                    buf.push(match iter.next()? {
                        '\\' => match iter.next()? {
                            '\'' => '\'',
                            '\\' => '\\',
                            '"' => '"',
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            _ => panic!("invalid escape")
                        },
                        '\n' => panic!("Lexer error (todo: handle)"),
                        '"' => break,
                        c => c,
                        //None => panic!("Lexer error (todo: handle)")
                    });
                }
                Some(Span::string(buf, iter.length(&start), start))
            },
            // other stuff
            _ => Some(Span::symbol(iter.next()?, start))
        }
    }
}
