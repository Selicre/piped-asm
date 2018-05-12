use byteorder::ByteOrder;
use parser::{Statement,Argument,ArgumentKind};
use std::io::{self, Write};


#[derive(Debug)]
pub enum CompileError {
    Instruction,
    AddressMode,
    WriteError(io::Error)
}

pub fn write_instr<W: Write>(mut w: W, instr: &str, arg: Argument) -> Result<(), CompileError> {
    use self::ArgumentKind::*;
    macro_rules! write {
        ($opcode:expr) => {{
            w.write_all(&[$opcode]).map_err(CompileError::WriteError)?;
            arg.write_to(w).map_err(CompileError::WriteError)?;
        }}
    }
    macro_rules! kinds {
        (Implied => $im:expr) => { match arg.kind { Implied => write!($im), _ => return Err(CompileError::AddressMode) } };

        (Implied => $im:expr, $($p:ident => $e:expr),*) => { match arg.kind { Implied => write!($im), $($p(_) => write!($e)),*, _ => return Err(CompileError::AddressMode) } };
        ($($p:ident => $e:expr),*) => { match arg.kind { $($p(_) => write!($e)),*, _ => return Err(CompileError::AddressMode) } };
    }
    macro_rules! move_mem {
        ($e:expr) => { match arg.kind { BlockMove(_,_) => write!($e), _ => return Err(CompileError::AddressMode) } }
    }
    macro_rules! implied {
        ($e:expr) => { kinds! { Implied => $e } }
    }
    macro_rules! relative {
        ($e:expr) => { kinds! { Relative => $e } }
    }
    macro_rules! common_arith {
        ($offset:expr) => { kinds! {
            DPIndX =>       0x01 + $offset,
            Stack =>        0x03 + $offset,
            DirectPage =>   0x05 + $offset,
            DPIndLong =>    0x07 + $offset,
            Immediate =>    0x09 + $offset,
            ImmediateWord =>0x09 + $offset,
            Absolute =>     0x0D + $offset,
            Long =>         0x0F + $offset,
            DPIndY =>       0x11 + $offset,
            DPInd =>        0x12 + $offset,
            StackY =>       0x13 + $offset,
            DPX =>          0x15 + $offset,
            DPIndLongY =>   0x17 + $offset,
            AbsoluteY =>    0x19 + $offset,
            AbsoluteX =>    0x1D + $offset,
            LongX =>        0x1F + $offset
        } }
    }
    macro_rules! common_implied {
        ($implied:expr, $offset:expr) => { kinds! {
            Implied => $implied,
            DirectPage => 0x06 + $offset,
            Absolute => 0x0E + $offset,
            DPX => 0x16 + $offset,
            AbsoluteX => 0x1E + $offset
        } };
        // shorthand
        ($offset:expr) => { common_implied!($offset + 0xA, $offset) }
    }
    // TODO: generate this from some db
    match instr {
        // ARITHMETIC
        "ADC" => common_arith!(0x60),
        "SBC" => common_arith!(0xE0),
        // COMPARISON
        "CMP" => common_arith!(0xC0),
        "CPX" => kinds! {
            Immediate => 0xE0,
            ImmediateWord => 0xE0,
            DirectPage => 0xE4,
            Absolute => 0xEC
        },
        "CPY" => kinds! {
            Immediate => 0xC0,
            ImmediateWord => 0xC0,
            DirectPage => 0xC4,
            Absolute => 0xCC
        },
        // INC/DECREMENTS
        "DEC" => common_implied!(0x3A, 0xC0),
        "DEX" => implied!(0xCA),
        "DEY" => implied!(0x88),
        "INC" => common_implied!(0x1A,0xE0),
        "INX" => implied!(0xE8),
        "INY" => implied!(0xC8),
        // BINOPS
        "AND" => common_arith!(0x20),
        "EOR" => common_arith!(0x40),
        "ORA" => common_arith!(0x00),
        // BIT STUFF
        "BIT" => kinds! {
            DirectPage =>   0x24,
            Absolute =>     0x2C,
            DPX =>          0x34,
            AbsoluteX =>    0x3C,
            Immediate =>    0x89,
            ImmediateWord =>0x89
        },
        "TRB" => kinds! {
            DirectPage =>   0x14,
            Absolute =>     0x1C
        },
        "TSB" => kinds! {
            DirectPage =>   0x04,
            Absolute =>     0x0C
        },
        "ASL" => common_implied!(0x00),
        "LSR" => common_implied!(0x40),
        "ROL" => common_implied!(0x20),
        "ROR" => common_implied!(0x60),
        // BRANCHES
        "BPL" => relative!(0x10),
        "BMI" => relative!(0x30),
        "BVC" => relative!(0x50),
        "BVS" => relative!(0x70),
        "BRA" => relative!(0x80),
        "BCC" => relative!(0x90),
        "BNE" => relative!(0xD0),
        "BCS" => relative!(0xB0),
        "BEQ" => relative!(0xF0),
        "BRL" => kinds! {
            RelativeWord => 0x82
        },
        // JUMPS
        "JMP" => kinds! {
            Absolute =>     0x4C,
            Long =>         0x5C,
            AbsInd =>       0x6C,
            AbsIndX =>      0x7C,
            AbsIndLong =>   0xDC
        },
        "JML" => kinds! {
            Long =>         0x5C,
            AbsIndLong =>   0xDC
        },
        "JSL" => kinds! {
            Long =>         0x22
        },
        "JSR" => kinds! {
            Absolute =>     0x20,
            AbsIndX =>      0xFC
        },
        "RTL" => implied!(0x6B),
        "RTS" => implied!(0x60),
        // INTERRUPTS - TODO: fix implied
        "BRK" => kinds! {
            Implied => 0x00,
            Immediate => 0x00
        },
        "COP" => kinds! {
            Implied => 0x02,
            Immediate => 0x02
        },
        "RTI" => implied!(0x40),
        "STP" => implied!(0xDB),
        "WAI" => implied!(0xCB),
        // CLEAR / SET FLAGS
        "CLC" => implied!(0x18),
        "CLD" => implied!(0xD8),
        "CLI" => implied!(0x58),
        "CLV" => implied!(0xB8),
        "SEC" => implied!(0x38),
        "SED" => implied!(0xF8),
        "SEI" => implied!(0x78),
        "REP" => kinds! {
            Immediate =>    0xC2
        },
        "SEP" => kinds! {
            Immediate =>    0xE2
        },
        // STORE / LOAD
        "STZ" => kinds! {
            DirectPage =>   0x64,
            DPX =>          0x74,
            Absolute =>     0x9C,
            AbsoluteX =>    0x9E
        },
        "STA" => kinds! {
            DPIndX =>       0x81,
            Stack =>        0x83,
            DirectPage =>   0x85,
            DPIndLong =>    0x87,
            Absolute =>     0x8D,
            Long =>         0x8F,
            DPIndY =>       0x91,
            DPInd =>        0x92,
            StackY =>       0x93,
            DPX =>          0x95,
            DPIndLongY =>   0x97,
            AbsoluteY =>    0x99,
            AbsoluteX =>    0x9D,
            LongX =>        0x9F
        },
        "STX" => kinds! {
            DirectPage =>   0x86,
            Absolute =>     0x8E,
            DPY =>          0x96
        },
        "STY" => kinds! {
            DirectPage =>   0x84,
            Absolute =>     0x8C,
            DPX =>          0x94
        },
        "LDA" => kinds! {
            DPIndX =>       0xA1,
            Stack =>        0xA3,
            DirectPage =>   0xA5,
            DPIndLong =>    0xA7,
            Immediate =>    0xA9,
            ImmediateWord =>0xA9,
            Absolute =>     0xAD,
            Long =>         0xAF,
            DPIndY =>       0xB1,
            DPInd =>        0xB2,
            StackY =>       0xB3,
            DPX =>          0xB5,
            DPIndLongY =>   0xB7,
            AbsoluteY =>    0xB9,
            AbsoluteX =>    0xBD,
            LongX =>        0xBF
        },
        "LDX" => kinds! {
            Immediate =>    0xA2,
            ImmediateWord =>0xA2,
            DirectPage =>   0xA6,
            Absolute =>     0xAE,
            DPY =>          0xB6,
            AbsoluteY =>    0xBE
        },
        "LDY" => kinds! {
            Immediate =>    0xA0,
            ImmediateWord =>0xA0,
            DirectPage =>   0xA4,
            Absolute =>     0xAC,
            DPX =>          0xB4,
            AbsoluteX =>    0xBC
        },
        // MOVE MEMORY
        "MVN" => move_mem!(0x54),
        "MVP" => move_mem!(0x44),
        // NOPS
        "NOP" => implied!(0xEA),
        "WDM" => kinds! {           // TODO: fix
            Implied =>      0x42,
            Immediate =>    0x42
        },
        // STACK STUFF
        "PEA" => kinds! {
            ImmediateWord =>0xF4,
            Absolute => 0xF4
        },
        "PEI" => kinds! {
            DPInd =>        0xD4
        },
        "PER" => kinds! {       // TODO: fix
            Relative => 0x62,
            RelativeWord => 0x62,
            Absolute => 0x62
        },
        "PHA" => implied!(0x48),
        "PHX" => implied!(0xDA),
        "PHY" => implied!(0x5A),
        "PLA" => implied!(0x68),
        "PLX" => implied!(0xFA),
        "PLY" => implied!(0x7A),

        "PHB" => implied!(0x8B),
        "PHD" => implied!(0x0B),
        "PHK" => implied!(0x4B),
        "PHP" => implied!(0x08),
        "PLB" => implied!(0xAB),
        "PLD" => implied!(0x2B),
        "PLP" => implied!(0x28),
        // TRANSFER
        "TAX" => implied!(0xAA),
        "TAY" => implied!(0xA8),
        "TSX" => implied!(0xBA),
        "TXA" => implied!(0x8A),
        "TXS" => implied!(0x9A),
        "TXY" => implied!(0x9B),
        "TYA" => implied!(0x98),
        "TYX" => implied!(0xBB),

        "TCD" => implied!(0x5B),
        "TCS" => implied!(0x1B),
        "TDC" => implied!(0x7B),
        "TSC" => implied!(0x3B),
        // EXCHANGE
        "XBA" => implied!(0xEB),
        "XCE" => implied!(0xFB),
        _ => return Err(CompileError::Instruction)
    }
    Ok(())
}
