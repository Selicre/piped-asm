use std::collections::VecDeque;
use std::ops;
use std::cell::Cell;
use std::fmt;
use std::io::{self,Write};

use lexer::{Span,SpanKind};

use byteorder::{WriteBytesExt,LittleEndian};

#[derive(Debug)]
pub enum ArgumentKind {
    Implied,
    Label(String),      // Label
    AnonLabel(i32),     // ++
    // The above can be removed if I wanna move labels to be processed at a later time
    Immediate(u8),      // #$A0
    ImmediateWord(u16), // #$1234
    DirectPage(u8),     // $12
    DPX(u8),            // $12,x
    DPY(u8),            // $12,y
    DPInd(u8),          // ($12)
    DPIndX(u8),         // ($12,x)
    DPIndY(u8),         // ($12),y
    DPIndLong(u8),      // [$12]
    DPIndLongY(u8),     // [$12],y
    Stack(u8),          // $12,s
    StackY(u8),         // ($12,s),y
    Absolute(u16),      // $1234
    AbsoluteX(u16),     // $1234,x
    AbsoluteY(u16),     // $1234,y
    AbsInd(u16),        // ($1234)
    AbsIndX(u16),       // ($1234,x)
    AbsIndLong(u16),    // [$1234]
    Long(u32),          // $7F1234
    LongX(u32),         // $7F1234,x
    Relative(i8),       // BRA label
    RelativeWord(i16),  // BRL label
    BlockMove(u8,u8)    // #$12,#$34
}

#[derive(Debug)]
pub struct Argument {
    pub kind: ArgumentKind,
    spans: Vec<Span>
}

impl Argument {
    fn parse(spans: Vec<Span>) -> Result<Self,ParseError> {
        use self::SpanKind::*;
        use self::ArgumentKind::*;
        let kind = {
            let kinds = spans.iter().map(|c| &c.kind).collect::<Vec<_>>();
            match &*kinds {
                &[] => Implied,
                &[Ident(ref s)] => Label(s.clone()),
                &[SpanKind::AnonLabel(c)] => ArgumentKind::AnonLabel(*c),
                &[Symbol('#'), Byte(c)] => Immediate(*c),
                &[Symbol('#'), Word(c)] => ImmediateWord(*c),
                &[Byte(c)] => DirectPage(*c),
                &[Byte(c), Symbol(','), Ident(x)] if &*x == "x" => DPX(*c),
                &[Byte(c), Symbol(','), Ident(y)] if &*y == "y" => DPY(*c),
                &[Symbol('('), Byte(c), Symbol(')')] => DPInd(*c),
                &[Symbol('('), Byte(c), Symbol(','), Ident(x), Symbol(')')] if &*x == "x" => DPIndX(*c),
                &[Symbol('('), Byte(c), Symbol(')'), Symbol(','), Ident(y)] if &*y == "y" => DPIndY(*c),
                &[Symbol('['), Byte(c), Symbol(']')] => DPIndLong(*c),
                &[Symbol('['), Byte(c), Symbol(']'), Symbol(','), Ident(y)] if &*y == "y" => DPIndLongY(*c),
                &[Byte(c), Symbol(','), Ident(s)] if &*s == "s" => Stack(*c),
                &[Symbol('('), Byte(c), Symbol(','), Ident(s), Symbol(')'), Symbol(','), Ident(y)] if &*s == "s" && &*y == "y" => StackY(*c),

                &[Word(c)] => Absolute(*c),
                &[Word(c), Symbol(','), Ident(x)] if &*x == "x" => AbsoluteX(*c),
                &[Word(c), Symbol(','), Ident(y)] if &*y == "y" => AbsoluteY(*c),
                &[Symbol('('), Word(c), Symbol(')')] => AbsInd(*c),
                &[Symbol('('), Word(c), Symbol(','), Ident(x), Symbol(')')] if &*x == "x" => AbsIndX(*c),
                &[Symbol('['), Word(c), Symbol(']')] => AbsIndLong(*c),
                &[SpanKind::Long(c)] => ArgumentKind::Long(*c),
                &[SpanKind::Long(c), Symbol(','), Ident(x)] if &*x == "x" => LongX(*c),
                &[Symbol('#'), Byte(c1), Symbol(','), Symbol('#'), Byte(c2)] => BlockMove(*c1, *c2),
                &[Byte(c1), Symbol(','), Byte(c2)] => BlockMove(*c1, *c2),
                _ => return Err(ParseError::GenericSyntaxError)
            }
        };
        Ok(Argument { kind, spans })
    }
    pub fn write_to<W: Write>(&self, mut w: W) -> io::Result<()> {
        use self::ArgumentKind::*;
        match self.kind {
            Implied => Ok(()),
            Immediate(c) => w.write_u8(c),      // #$A0
            ImmediateWord(c) => w.write_u16::<LittleEndian>(c), // #$1234
            DirectPage(c) => w.write_u8(c),     // $12
            DPX(c) => w.write_u8(c),            // $12,x
            DPY(c) => w.write_u8(c),            // $12,y
            DPInd(c) => w.write_u8(c),          // ($12)
            DPIndX(c) => w.write_u8(c),         // ($12,x)
            DPIndY(c) => w.write_u8(c),         // ($12),y
            DPIndLong(c) => w.write_u8(c),      // [$12]
            DPIndLongY(c) => w.write_u8(c),     // [$12],y
            Stack(c) => w.write_u8(c),          // $12,s
            StackY(c) => w.write_u8(c),         // ($12,s),y
            Absolute(c) => w.write_u16::<LittleEndian>(c),      // $1234
            AbsoluteX(c) => w.write_u16::<LittleEndian>(c),     // $1234,x
            AbsoluteY(c) => w.write_u16::<LittleEndian>(c),     // $1234,y
            AbsInd(c) => w.write_u16::<LittleEndian>(c),        // ($1234)
            AbsIndX(c) => w.write_u16::<LittleEndian>(c),       // ($1234,x)
            AbsIndLong(c) => w.write_u16::<LittleEndian>(c),    // [$1234]
            Long(c) => w.write_u24::<LittleEndian>(c),          // $7F1234
            LongX(c) => w.write_u24::<LittleEndian>(c),         // $7F1234,x
            BlockMove(c1, c2) => { w.write_u8(c1)?; w.write_u8(c2) },   // #$12,#$34
            Relative(c) => w.write_i8(c),       // ($1234,x)
            RelativeWord(c) => w.write_i16::<LittleEndian>(c),    // [$1234]
            _ => panic!("compiler broke: wrong context for write_to")
        }
    }
}

impl fmt::Display for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ArgumentKind::*;
        match self.kind {
            Implied => write!(f, "implied"),
            Label(ref s) => write!(f, "label {}", s),
            Immediate(b) => write!(f, "Immediate byte #${:02X}", b),
            ImmediateWord(w) => write!(f, "Immediate word #${:04X}", w),
            DirectPage(b) => write!(f, "Direct page ${:02X}", b),
            _ => write!(f, "something else")
        }
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
        if let SpanKind::Ident(ref t) = s.kind {
            Ok((match &**t {
                "l" => OpSize::Long,
                "w" => OpSize::Word,
                "b" => OpSize::Byte,
                _ => return Err(ParseError::InvalidOpSize)
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
    Attributes {
        attrs: Vec<Span>
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match self {
            Label { name } => write!(f, "Label {}", name),
            Instruction { name, size, arg } => write!(f, "Instruction: {}, argument: {:?}", name, arg.kind),
            Attributes { attrs } => write!(f, "Attributes (unused yet)"),
            _ => write!(f, "Unimplemented!")
        }
    }
}


#[derive(Debug)]
pub enum ParseError {
    UnknownSpan,
    InvalidOpSize,
    GenericSyntaxError
}

// Easier to match on this, also todo: deref
#[derive(Default)]
struct SpanQueue {
    spans: VecDeque<Span>,
    kind_buf: Cell<Option<Vec<&'static SpanKind>>>  // dubious performance gain but yay unsafety
}

impl SpanQueue {
    fn push(&mut self, s: Span) {
        self.spans.push_back(s);
    }
    fn autoclean(&mut self) -> QueueClearHandle {
        QueueClearHandle(self)
    }
    fn clear(&mut self) {
        self.spans.clear();
    }
    fn queue<'a>(&'a self) -> &'a [&'a SpanKind] {
        let mut b = self.kind_buf.take().unwrap_or_default();
        b.clear();
        b.extend(self.spans.iter().map(|c| unsafe { ::std::mem::transmute(&c.kind): &'static SpanKind }));
        let c = unsafe { ::std::mem::transmute(&*b): &'a [&'a SpanKind] };
        self.kind_buf.set(Some(b));
        c
    }
}
// convenience for clearing the struct
struct QueueClearHandle<'a>(&'a mut SpanQueue);
impl<'a> Drop for QueueClearHandle<'a> {
    fn drop(&mut self) {
        self.0.clear();
    }
}
impl<'a> ops::Deref for QueueClearHandle<'a> {
    type Target = SpanQueue;
    fn deref(&self) -> &SpanQueue {
        &*self.0
    }
}
impl<'a> ops::DerefMut for QueueClearHandle<'a> {
    fn deref_mut(&mut self) -> &mut SpanQueue {
        self.0
    }
}

pub struct Parser<S> {
    iter: S,
    buf: SpanQueue
}
impl<S: Iterator<Item=Span>> Parser<S> {
    pub fn new(iter: S) -> Self {
        Self { iter, buf: Default::default() }
    }
}
impl<S: Iterator<Item=Span>> Iterator for Parser<S> {
    type Item = Result<Statement,ParseError>;
    fn next(&mut self) -> Option<Result<Statement,ParseError>> {
        use self::SpanKind::*;
        use self::Statement::*;
        let mut buf = self.buf.autoclean();
        while let Some(c) = self.iter.next() {
            //println!("span {}",c);
            buf.push(c);
            match match buf.queue() {
                // Label:
                &[Ident(_), Symbol(':')] |
                &[AnonLabel(_)] =>
                    Some(Label { name: buf.spans[0].clone() }),
                &[Ident(_), LineBreak] |
                &[Ident(_), Whitespace, Symbol(':')] => Some(Instruction {
                    name: buf.spans[0].clone(),
                    size: None,
                    arg: Argument {
                        kind: ArgumentKind::Implied,
                        spans: Vec::new()
                    }
                }),
                &[Ident(_), Whitespace, ref rest.., LineBreak] |
                &[Ident(_), Whitespace, ref rest.., Whitespace, Symbol(':')] => Some(Instruction {
                    name: buf.spans[0].clone(),
                    size: None,
                    arg: match Argument::parse(buf.spans.iter()
                                         .skip(2).take(rest.len())
                                         .filter(|c| !c.is_whitespace())
                                         .cloned().collect()) {
                        Ok(c) => c,
                        Err(e) => return Some(Err(e))
                    }
                }),
                &[Ident(_), Symbol('.'), Ident(_), Whitespace, ref rest.., LineBreak] |
                &[Ident(_), Symbol('.'), Ident(_), Whitespace, ref rest.., Whitespace, Symbol(':')] => Some(Instruction {
                    name: buf.spans[0].clone(),
                    size: Some(OpSize::parse(&buf.spans[2]).unwrap()),   // todo: fix panic
                    arg: match Argument::parse(buf.spans.iter()
                                         .skip(4).take(rest.len())
                                         .filter(|c| !c.is_whitespace())
                                         .cloned().collect()) {
                        Ok(c) => c,
                        Err(e) => return Some(Err(e))
                    }
                }),
                &[Symbol('#'), Symbol('['), .., Symbol(']')] => Some(Attributes {
                    attrs: buf.spans.iter().cloned().collect()
                }),
                &[Whitespace] | &[LineBreak] => None,
                &[.., LineBreak] => return Some(Err(ParseError::GenericSyntaxError)),
                _ => continue
            } {
                Some(c) => return Some(Ok(c)),
                None => buf.clear()
            }
        }
        if buf.spans.len() > 0 {
            return Some(Err(ParseError::GenericSyntaxError))
        }
        None
    }
}
