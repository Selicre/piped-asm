use parser::{Argument,ArgumentKind};
use lexer::Span;
use byteorder::LittleEndian;
use std::io::Write;
use byteorder::WriteBytesExt;
use std::io;
use instructions::SizeHint;
use expression::ExprNode;

#[derive(Debug)]
pub enum AddressingMode {
    Implied,
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
    AbsLong(u32),       // $7F1234
    AbsLongX(u32),      // $7F1234,x
    Relative(i8),       // BRA label
    RelativeWord(i16),  // BRL label
    BlockMove(u8,u8)    // #$12,#$34
}

/*
#[derive(Debug)]
pub enum Argument {
    Implied,
    Constant(Span),
    Direct(Span),
    IndexedX(Span),
    IndexedY(Span),
    Indirect(Span),
    IndX(Span),
    IndY(Span),
    IndLong(Span),
    IndLongY(Span),
    Stack(Span),
    StackY(Span),
    TwoArgs(Span,Span)
}
*/
#[derive(Debug)]
pub struct AddrModeError;

impl AddressingMode {
    // TODO: pass out label info
    pub fn parse(arg: Argument, hint: SizeHint) -> Result<Self,AddrModeError> {
        use self::AddressingMode::*;
        use self::ArgumentKind::*;
        use self::SizeHint::*;
        if let TwoArgs(Span::Byte(c1), Span::Byte(c2)) = arg.kind {
            return Ok(BlockMove(c1.data as u8, c2.data as u8));
        }
        let mut label_is_a = false;
        let val = match *arg.expr {
            ExprNode::Constant(val) => val,
            ExprNode::Label(ref c) if &*c == "A" || &*c == "a" => {
                label_is_a = true;
                //println!("label A");
                0
            }
            _ => 0
        };
        let h = Byte.and_then(hint);
        Ok(match (arg.kind,h) {
            (Direct,_) if label_is_a => AddressingMode::Implied,
            (Constant,Byte) => Immediate(val as u8),
            (Constant,Word) => ImmediateWord(val as u16),
            (Direct,Byte) => DirectPage(val as u8),
            (IndexedX,Byte) => DPX(val as u8),
            (IndexedY,Byte) => DPY(val as u8),
            (Indirect,Byte) => DPInd(val as u8),
            (IndX,Byte) => DPIndX(val as u8),
            (IndY,Byte) => DPIndY(val as u8),
            (IndLong,Byte) => DPIndLong(val as u8),
            (IndLongY,Byte) => DPIndLongY(val as u8),
            (ArgumentKind::Stack,Byte) => AddressingMode::Stack(val as u8),
            (ArgumentKind::StackY,Byte) => AddressingMode::StackY(val as u8),

            (Direct,Word) => Absolute(val as u16),
            (IndexedX,Word) => AbsoluteX(val as u16),
            (IndexedY,Word) => AbsoluteY(val as u16),
            (Indirect,Word) => AbsInd(val as u16),
            (IndX,Word) => AbsIndX(val as u16),
            (IndLong,Word) => AbsIndLong(val as u16),
            (Direct,Long) => AbsLong(val as u32),
            (IndexedX,Long) => AbsLongX(val as u32),

            (Direct,RelByte) => Relative(val as i8),
            (Direct,RelWord) => RelativeWord(val as i16),

            (ArgumentKind::Implied,_) => AddressingMode::Implied,

            (c,v) => { println!("{:?} and {:?}", c, v); return Err(AddrModeError) }
        })
    }
    pub fn write_to<W: Write>(&self, mut w: W) -> io::Result<()> {
        use self::AddressingMode::*;
        match self {
            Implied => Ok(()),
            Immediate(c) => w.write_u8(*c),      // #$A0
            ImmediateWord(c) => w.write_u16::<LittleEndian>(*c), // #$1234
            DirectPage(c) => w.write_u8(*c),     // $12
            DPX(c) => w.write_u8(*c),            // $12,x
            DPY(c) => w.write_u8(*c),            // $12,y
            DPInd(c) => w.write_u8(*c),          // ($12)
            DPIndX(c) => w.write_u8(*c),         // ($12,x)
            DPIndY(c) => w.write_u8(*c),         // ($12),y
            DPIndLong(c) => w.write_u8(*c),      // [$12]
            DPIndLongY(c) => w.write_u8(*c),     // [$12],y
            Stack(c) => w.write_u8(*c),          // $12,s
            StackY(c) => w.write_u8(*c),         // ($12,s),y
            Absolute(c) => w.write_u16::<LittleEndian>(*c),      // $1234
            AbsoluteX(c) => w.write_u16::<LittleEndian>(*c),     // $1234,x
            AbsoluteY(c) => w.write_u16::<LittleEndian>(*c),     // $1234,y
            AbsInd(c) => w.write_u16::<LittleEndian>(*c),        // ($1234)
            AbsIndX(c) => w.write_u16::<LittleEndian>(*c),       // ($1234,x)
            AbsIndLong(c) => w.write_u16::<LittleEndian>(*c),    // [$1234]
            AbsLong(c) => w.write_u24::<LittleEndian>(*c),          // $7F1234
            AbsLongX(c) => w.write_u24::<LittleEndian>(*c),         // $7F1234,x
            BlockMove(c1, c2) => { w.write_u8(*c1)?; w.write_u8(*c2) },   // #$12,#$34
            Relative(c) => w.write_i8(*c),       // ($1234,x)
            RelativeWord(c) => w.write_i16::<LittleEndian>(*c),    // [$1234]
        }
    }
}
