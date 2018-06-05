// output the actual file
// Note: this may use streaming too but I honestly just don't care enough for that


use std::io::Write;
use std::io::Seek;
use std::io::SeekFrom;
use std::io::Cursor;

use std::borrow::Cow;

use std::error::Error;

use std::collections::HashMap;

use byteorder::WriteBytesExt;
use byteorder::LittleEndian;

use linked_hash_map::LinkedHashMap;

use compiler::LabeledChunk;

use instructions::SizeHint;

use expression::ExprNode;

// Anything that isn't directly bank data (lorom mode, etc.), also TODO
struct BankContext {
    
}

#[derive(Default)]
struct Bank {
    // also used as current position
    size: usize,
    chunks: LinkedHashMap<String,(usize,LabeledChunk)>,
    // todo: pinned chunks
}

impl Bank {
    fn append(&mut self, label: String, chunk: LabeledChunk) -> Option<usize> {
        let size = chunk.size();
        println!("{}: ${:04X}", label, self.size);
        self.chunks.insert(label, (self.size, chunk));
        self.size += size;
        Some(self.size)
    }
    fn fits(&self, bank: u8, chunk: &LabeledChunk) -> bool {
        // lorom only
        // Currently, header is very hacked together. In the future, the header will be pinned to
        // 0x7FC0 automatically and exist in the same space as the rest of the chunks
                                   // Make space for the header
        self.size + chunk.size() < if bank == 0 { 0x7FC0 } else { 0x8000 }
    }
}

struct Banks {
    content: Vec<Bank>,
    refs: HashMap<String, (u8, usize)>
}

impl Banks {
    fn new() -> Self {
        Self {
            // TODO: scale the rom accordingly
            content: (0..16).map(|_| Bank::default()).collect(),
            refs: Default::default()
        }
    }
    fn append(&mut self, label: String, chunk: LabeledChunk) -> Option<usize> {
        let (bank_id, bank) = match chunk.bank_hint {
            Some(c) => (c as usize, &mut self.content[c as usize]),
            None => {   // for shit like JSL routines
                // find an appropriate bank
                self.content.iter_mut().enumerate().find(|x| x.1.fits(x.0 as u8, &chunk))?
            }
        };
        self.refs.insert(label.clone(), (bank_id as u8, bank.size));
        bank.append(label, chunk);
        Some(bank.size)
    }
    fn seal(&mut self, bank: u8) {
        // lorom only, again
        self.content[bank as usize].size = 0x8000;
    }
    fn write_to<W: Write>(&mut self, mut w: W) -> Result<(), Box<Error>> {
        let refs = &self.refs;
        for (ref bank_id, ref mut bank) in self.content.iter_mut().enumerate() {
            let mut bank_contents = Vec::with_capacity(0x8000);
            for (label, (offset, chunk)) in bank.chunks.iter_mut() {
                let mut c = Cursor::new(chunk.data.clone());    // Cow?
                for (expr_offset,expr) in chunk.pending_exprs.iter_mut() {
                    let abs_offset = *offset + *expr_offset;
                    expr.each_mut(|c| {
                        use expression::ExprNode::*;
                        match c {
                            Label(d) => *c = ExprNode::Constant({let (b,c) = refs[d]; (b as i32)*0x10000+0x8000 + c as i32}),
                            LabelOffset(d) => *c = ExprNode::Constant((abs_offset as i32)+(*d as i32)),
                            _ => {}
                        }
                    });
                    expr.reduce();
                    let mut val = if let ExprNode::Constant(c) = expr.root { c } else { panic!("Can't collapse expression {}", expr.root) };
                    println!("Reduced expression to {:04X} (offset: {:X}, expr_offset: {:X})", val, offset, expr_offset);
                    c.seek(SeekFrom::Start(*expr_offset as u64)).unwrap();
                    match expr.size {
                        SizeHint::Byte => c.write_u8(val as u8).unwrap(),
                        SizeHint::Word => c.write_u16::<LittleEndian>(val as u16).unwrap(),
                        SizeHint::Long => c.write_u24::<LittleEndian>(val as u32).unwrap(),
                        SizeHint::RelByte => c.write_i8((val - abs_offset as i32 - 1) as i8).unwrap(),
                        SizeHint::RelWord => c.write_i16::<LittleEndian>((val - abs_offset as i32 - 1) as i16).unwrap(),
                        c => panic!("oh no what is this size {:?}", c)
                    }
                }
                /*for i in panic!() { //chunk.label_refs() {
                    // todo: remove panic
                    let (bank, val) = self.refs[&i.label];
                    let mut val = val as isize;
                    c.seek(SeekFrom::Start(i.offset as u64 + 1)).unwrap();
                    // TODO: why 2?
                    let rel_offset = |val| val - (offset + i.offset + 2) as isize;
                    match i.size {
                        SizeHint::Byte => c.write_u8(val as u8),
                        SizeHint::Word => c.write_u16::<LittleEndian>(val as u16 + 0x8000),
                        // TODO: figure out banks
                        SizeHint::Long => {
                            // also todo: calculate this based on lorom/hirom/etc.
                            c.write_u16::<LittleEndian>(val as u16 + 0x8000)?;
                            c.write_u8(bank)
                        },
                        SizeHint::RelByte => c.write_i8(rel_offset(val) as i8),
                        SizeHint::RelWord => c.write_i16::<LittleEndian>(rel_offset(val) as i16),
                        _ => panic!("linker error lel")
                    }?;
                }*/
                bank_contents.write_all(&c.get_ref())?;
            }
            if *bank_id == 0 {
                let (b, start) = self.refs["Start"];
                if b != 0 { Err("bank for Start not 0")?; }
                let (b, nmi) = self.refs["VBlank"];
                if b != 0 { Err("bank for VBlank not 0")?; }
                let (b, irq) = self.refs.get("IRQ").unwrap_or(&(0, 0x7FFF)).clone();
                if b != 0 { Err("bank for IRQ not 0")?; }
                let h = header(start as u16, nmi as u16, irq as u16);
                bank_contents.resize(0x7FC0, 0x00);
                bank_contents.extend_from_slice(&h.data);
            } else {
                bank_contents.resize(0x8000, 0x00);
            }
            w.write_all(&bank_contents)?;
        }
        Ok(())
    }
}

fn header(entry: u16, nmi: u16, irq: u16) -> LabeledChunk {
    ((|| {
    let mut chunk = LabeledChunk::default();
    // LOROM only, for now
    chunk.pin(0x7FC0);
    // 21 bytes:       ---------------------
    chunk.data.write_all(b"SUPER MARIOWORLD     ")?;
    chunk.data.write_all(&[
    // ROM makeup, type, size (TODO!), SRAM size, destination code
        0x23, 0x00, 0x0C, 0x07, 0x01, 0x33 ])?;
    chunk.data.write_u8(0x00)?;                      // Version
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // ROM checksum complement stub
    chunk.data.write_u16::<LittleEndian>(0x0000)?;   // ROM checksum stub
    // interrupt vectors (TODO)
    // NATIVE
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // COP enable
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // BRK
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // ABORT
    chunk.data.write_u16::<LittleEndian>(nmi + 0x8000)?;      // NMI
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // RESET (unused)
    chunk.data.write_u16::<LittleEndian>(irq + 0x8000)?;   // IRQ
    // EMULATION
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // COP enable
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // unused
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // ABORT
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // NMI
    chunk.data.write_u16::<LittleEndian>(entry + 0x8000)?;    // RESET (execution begins here)
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // IRQ
    assert_eq!(chunk.data.len(), 0x40);
    Ok(chunk) })(): Result<_,Box<Error>>).unwrap()
}


pub fn link<W: Write, I: Iterator<Item=(String,LabeledChunk)>>(writer: W, iter: I) {
    let mut banks = Banks::new();
    for (name,block) in iter {
        banks.append(name,block);
    }
    banks.write_to(writer).unwrap();
}
