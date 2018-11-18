// output the actual file
// Note: this may use streaming too but I honestly just don't care enough for that


use std::io::Write;
use std::io::Seek;
use std::io::SeekFrom;
use std::io::Cursor;

use std::time::Instant;

use std::error::Error;

use std::collections::{HashMap,HashSet};

use byteorder::WriteBytesExt;
use byteorder::LittleEndian;

use linked_hash_map::LinkedHashMap;

use compiler::{CompileData,LabeledChunk};

use instructions::SizeHint;

use expression::{Expression,ExprNode};

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
    fn is_clear(&self) -> bool {
        self.size == 0
    }
    // In the future, it will be possible to pin chunks to addresses after $00, and data from
    // this bank may be adjusted to fit automatically.
    // For now, it's literally just whether the bank is clean
    fn fits_head(&self, chunk: &LabeledChunk) -> bool {
        self.size == 0
    }
}

struct Banks {
    content: Vec<Bank>,
    // todo: an _actual_ label graph
    last_bank: u8,
    refs: HashMap<String, (u8, usize)>,
    defines: HashMap<String, Expression>
}

impl Banks {
    fn new() -> Self {
        Self {
            // TODO: scale the rom accordingly
            content: (0..32).map(|_| Bank::default()).collect(),
            last_bank: 0,
            defines: Default::default(),
            refs: Default::default()
        }
    }
    fn add_define(&mut self, label: String, expr: Expression) {
        self.defines.insert(label, expr);
    }
    fn append_spanning_chunk(&mut self, label: String, chunk: LabeledChunk) -> Option<usize> {
        // LOROM only
        let req_empty_banks = chunk.size()/0x8000;  // How many full banks does this data take?
        if req_empty_banks > 0 {
            panic!("I don't support chunks larger than 32k yet");
            //self.content.windows(req_empty_banks)
            // ...
        }
        panic!();

    }
    fn append_chunk(&mut self, label: String, chunk: LabeledChunk) -> Option<usize> {
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
        let addr = (bank_id as u8, bank.size + 0x8000);
        for i in chunk.attrs.iter() {
            use attributes::Attribute::*;
            match i {
                Start => { self.refs.insert("*Start".to_string(), addr); },
                NMI =>   { self.refs.insert("*NMI".to_string(), addr); },
                IRQ =>   { self.refs.insert("*IRQ".to_string(), addr); },
                BRK =>   { self.refs.insert("*BRK".to_string(), addr); },
                _ => {}
            };
        }
        self.refs.insert(label.clone(), addr);
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
        let defines = &self.defines;
        let mut groups = HashMap::<String,Vec<String>>::new();
        let mut fallthrough_last = None;
        for (ref bank_id, ref mut bank) in self.content.iter_mut().enumerate() {
            let mut bank_contents = Vec::with_capacity(0x8000);
            for (label, (offset, chunk)) in bank.chunks.iter_mut() {
                let mut c = Cursor::new(chunk.data.clone());    // Cow?
                let mut word_refs = Vec::new();
                if let Some(c) = fallthrough_last.take() {
                    groups.get_mut(&c).unwrap().push(label.to_string());
                }
                if !chunk.diverging { fallthrough_last = Some(label.clone()) }
                for mut r in chunk.pending_exprs.iter_mut() {
                    let (expr_offset,expr) = (r.offset,&mut r.expr);
                    let abs_offset = *offset + expr_offset;
                    let mut size = expr.size;
                    for i in 0.. {
                        expr.each_mut(|c| {
                            match c {
                                ExprNode::Label(d) => if let Some(e) = defines.get(d) {
                                    *c = e.root.clone();
                                    //size = size.max(e.size);
                                },
                                _ => {}
                            }
                        });
                        // if can't reduce any more, good
                        if !expr.reduce() { break; }
                        if i > 64 { panic!("Recursion too deep in {} (64 max)", label) }
                    }
                    expr.each_mut(|c| {
                        use expression::ExprNode::*;
                        match c {
                            Label(d) => {
                                *c = ExprNode::Constant({
                                    if size == SizeHint::Word || size == SizeHint::RelByte || size == SizeHint::RelWord {
                                        word_refs.push(d.to_string());
                                    }
                                    let (b,c) = refs.get(d).unwrap_or_else(|| panic!("not found label: {}", d));
                                    (*b as i32)*0x10000 + *c as i32 + 0x800000
                                })
                            },
                            LabelOffset(d) => {
                                *c = ExprNode::Constant(0x8000 + (*offset as i32)+(*d as i32))
                            },
                            _ => {}
                        }
                    });
                    expr.reduce();
                    let mut val = if let ExprNode::Constant(c) = expr.root { c } else { panic!("Can't collapse expression {} in {}", expr.root, label) };
                    c.seek(SeekFrom::Start(expr_offset as u64)).unwrap();
                    match size {
                        SizeHint::Byte => {
                            //if val > 0xFF { println!("WARNING: expression out of range for byte size"); }
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
                groups.insert(label.to_string(), word_refs);
                bank_contents.write_all(&c.get_ref())?;
            }

            if *bank_id == 0 {
                let (b, start) = self.refs["*Start"];
                if b != 0 { Err("bank for Start not 0")?; }
                let (b, nmi) = self.refs["*NMI"];
                if b != 0 { Err("bank for VBlank not 0")?; }
                let (b, irq) = self.refs.get("*IRQ").unwrap_or(&(0, 0x7FFF)).clone();
                if b != 0 { Err("bank for IRQ not 0")?; }
                let (b, brk) = self.refs.get("*BRK").unwrap_or(&(0, 0x7FFF)).clone();
                if b != 0 { Err("bank for BRK not 0")?; }
                let h = header(start as u16, nmi as u16, irq as u16, brk as u16);
                bank_contents.resize(0x7FC0, 0x00);
                bank_contents.extend_from_slice(&h.data);
            } else {
                bank_contents.resize(0x8000, 0x00);
            }
            w.write_all(&bank_contents)?;
        }
        //let mut f = ::std::fs::File::create("out-graph.json").unwrap();
        //writeln!(f, "{:?}", groups);
        Ok(())
    }
}

fn header(entry: u16, nmi: u16, irq: u16, brk: u16) -> LabeledChunk {
    ((|| {
    let mut chunk = LabeledChunk::default();
    // LOROM only, for now
    chunk.pin(0x7FC0);
    let mut title = "SUPER MARIOWORLD     ".as_bytes().to_vec();
    title.resize(21, b' ');
    chunk.data.write_all(&title)?;
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
    chunk.data.write_u16::<LittleEndian>(brk)?;   // BRK
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

pub fn link<W: Write, I: Iterator<Item=CompileData>>(writer: W, iter: I) {
    let mut banks = Banks::new();
    let mut now = Instant::now();
    for c in iter {
        match c {
            CompileData::Chunk { label, chunk } => {
                let len = chunk.data.len();
                let c = banks.append_chunk(label.clone(),chunk);
                match c {
                    Some(a) => {
                        println!("[\x1B[38;5;117m{: >7}µs\x1B[0m] {: >24}: $\x1B[38;5;118m{:06X}\x1B[0m (size: \x1B[38;5;118m{:04X}\x1B[0m)", micros(now), label, a, len);
                    },
                    None => println!("WARNING: can't fit label {} (size {})", label,len)
                }
                now = Instant::now();
            },
            CompileData::Define { label, expr } => {
                banks.add_define(label, expr);
            },
            c => panic!("{:?}",c) // todo: handle
        }
    }
    println!("Writing..");
    let now = Instant::now();
    banks.write_to(writer).unwrap();
    println!("Done in {}µs", micros(now));
}
