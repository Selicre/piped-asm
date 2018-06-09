// output the actual file
// Note: this may use streaming too but I honestly just don't care enough for that


use std::io::Write;
use std::io::Seek;
use std::io::SeekFrom;
use std::io::Cursor;

use std::time::Instant;

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
        //println!("{}: ${:04X}", label, self.size);
        self.chunks.insert(label, (self.size, chunk));
        self.size += size;
        Some(self.size)
    }
    fn fits(&self, bank: u8, chunk: &LabeledChunk) -> bool {
        // lorom only
        // Currently, header is very hacked together. In the future, the header will be pinned to
        // 0x7FC0 automatically and exist in the same space as the rest of the chunks
                                   // Make space for the header
        self.size + chunk.size() <= if bank == 0 { 0x7FC0 } else { 0x8000 }
    }
}

struct Banks {
    content: Vec<Bank>,
    // todo: an _actual_ label graph
    last_bank: u8,
    refs: HashMap<String, (u8, usize)>
}

impl Banks {
    fn new() -> Self {
        Self {
            // TODO: scale the rom accordingly
            content: (0..32).map(|_| Bank::default()).collect(),
            last_bank: 0,
            refs: Default::default()
        }
    }
    fn append(&mut self, label: String, chunk: LabeledChunk) -> Option<usize> {
        let (bank_id, bank) = match chunk.bank_hint {
            Some(c) => {
                let bank = &mut self.content[c as usize];
                if !bank.fits(c, &chunk) { return None; }
                (c as usize, bank)
            },
            None => {   // for shit like JSL routines
                // find an appropriate bank
                self.content.iter_mut().enumerate().skip(self.last_bank as usize).find(|x| x.1.fits(x.0 as u8, &chunk))?
            }
        };

        self.last_bank = bank_id as u8;
        self.refs.insert(label.clone(), (bank_id as u8, bank.size + 0x8000));
        let addr = bank_id * 0x10000 + bank.size + 0x8000;
        bank.append(label, chunk);
        Some(addr)
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
                    //println!("reducing expression at ${:04X}: {}", abs_offset, expr);
                    expr.each_mut(|c| {
                        use expression::ExprNode::*;
                        match c {
                            Label(d) => *c = ExprNode::Constant({
                                let (b,c) = refs.get(d).unwrap_or_else(|| panic!("not found label: {}", d));
                                (*b as i32)*0x10000 + *c as i32
                            }),
                            LabelOffset(d) => *c = ExprNode::Constant(0x8000 + (*offset as i32)+(*d as i32)),
                            _ => {}
                        }
                    });
                    expr.reduce();
                    let mut val = if let ExprNode::Constant(c) = expr.root { c } else { panic!("Can't collapse expression {}", expr.root) };
                    //println!("Reduced expression to {:04X}", val);
                    c.seek(SeekFrom::Start(*expr_offset as u64)).unwrap();
                    match expr.size {
                        SizeHint::Byte => {
                            //if val > 0xFF { println!("WARNING: expression out of range for word size"); }
                            c.write_u8(val as u8).unwrap()
                        },
                        SizeHint::Word => {
                            //if val > 0xFFFF { println!("WARNING: expression out of range for word size"); }
                            c.write_u16::<LittleEndian>(val as u16).unwrap()
                        },
                        SizeHint::Long => c.write_u24::<LittleEndian>(val as u32).unwrap(),
                        SizeHint::RelByte => c.write_i8((val - abs_offset as i32 - 1) as i8).unwrap(),
                        SizeHint::RelWord => c.write_i16::<LittleEndian>((val - abs_offset as i32 - 2 - 0x8000) as i16).unwrap(),
                        c => panic!("oh no what is this size {:?}", c)
                    }
                }
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
        0x20, 0x02, 0x10, 0x01, 0x01, 0x01 ])?;
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
    chunk.data.write_u16::<LittleEndian>(nmi)?;      // NMI
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // RESET (unused)
    chunk.data.write_u16::<LittleEndian>(irq)?;   // IRQ
    // EMULATION
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // COP enable
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // unused
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // ABORT
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // NMI
    chunk.data.write_u16::<LittleEndian>(entry)?;    // RESET (execution begins here)
    chunk.data.write_u16::<LittleEndian>(0xFFFF)?;   // IRQ
    assert_eq!(chunk.data.len(), 0x40);
    Ok(chunk) })(): Result<_,Box<Error>>).unwrap()
}

fn micros(now: Instant) -> u64 {
    let elapsed = now.elapsed();
    elapsed.as_secs()*1000000 + elapsed.subsec_nanos() as u64/1000
}

pub fn link<W: Write, I: Iterator<Item=(String,LabeledChunk)>>(writer: W, iter: I) {
    let mut banks = Banks::new();
    let mut now = Instant::now();
    for (name,block) in iter {
        let len = block.data.len();
        let c = banks.append(name.clone(),block);
        match c {
            Some(a) => {
                println!("[\x1B[38;5;117m{: >7}µs\x1B[0m] {: >24}: $\x1B[38;5;118m{:06X}\x1B[0m (size: \x1B[38;5;118m{:04X}\x1B[0m)", micros(now), name, a, len);
            },
            None => println!("WARNING: can't fit label {} (size {})", name,len)
        }
        now = Instant::now();
    }
    println!("Writing..");
    let now = Instant::now();
    banks.write_to(writer).unwrap();
    println!("Done in {}µs", micros(now));
}
