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
    fn write_to<W: Write>(&self, mut w: W) -> Result<(), Box<Error>> {
        for (bank_id, bank) in self.content.iter().enumerate() {
            let mut bank_contents = Vec::with_capacity(0x8000);
            for (label, (offset, chunk)) in bank.chunks.iter() {
                let mut c = Cursor::new(chunk.data.clone());    // Cow?
                for i in chunk.label_refs() {
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
                        SizeHint::Long => c.write_u24::<LittleEndian>(val as u32 + 0x8000),
                        SizeHint::RelByte => c.write_i8(rel_offset(val) as i8),
                        SizeHint::RelWord => c.write_i16::<LittleEndian>(rel_offset(val) as i16),
                        _ => panic!("linker error lel")
                    }?;
                }
                bank_contents.write_all(&c.get_ref())?;
            }
            if bank_id == 0 {
                let (b, start) = self.refs["Start"];
                if b != 0 { Err("bank for Start not 0")?; }
                let (b, nmi) = self.refs["VBlank"];
                if b != 0 { Err("bank for VBlank not 0")?; }
                let h = header(start as u16, nmi as u16);
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

fn header(entry: u16, nmi: u16) -> LabeledChunk {
    let mut chunk = LabeledChunk::default();
    // LOROM only, for now
    chunk.pin(0x7FC0);
    // 21 bytes:       ---------------------
    chunk.data.write(b"SUPER MARIOWORLD     ");
    chunk.data.write(&[
    // ROM makeup, type, size (TODO!), SRAM size, creator ID (2 bytes)
        0x23, 0x35, 0x0C, 0x07, 0x01, 0x01,
    // Version #, checksum complement (2 bytes), checksum (2 bytes)
        0x00, 0x2D, 0x67, 0xD2, 0x98 ]);
    // interrupt vectors (TODO)
    // NATIVE
    chunk.data.write_u16::<LittleEndian>(0xFFFF);
    chunk.data.write_u16::<LittleEndian>(0xFFFF);
    chunk.data.write_u16::<LittleEndian>(0xFFFF);   // COP enable
    chunk.data.write_u16::<LittleEndian>(0xFFFF);   // BRK
    chunk.data.write_u16::<LittleEndian>(0xFFFF);   // ABORT
    chunk.data.write_u16::<LittleEndian>(nmi + 0x8000);      // NMI
    chunk.data.write_u16::<LittleEndian>(0xFFFF);   // RESET (unused)
    chunk.data.write_u16::<LittleEndian>(0xFFFF);   // IRQ
    // EMULATION
    chunk.data.write_u16::<LittleEndian>(0xFFFF);
    chunk.data.write_u16::<LittleEndian>(0xFFFF);
    chunk.data.write_u16::<LittleEndian>(0xFFFF);   // COP enable
    chunk.data.write_u16::<LittleEndian>(0xFFFF);   // unused
    chunk.data.write_u16::<LittleEndian>(0xFFFF);   // ABORT
    chunk.data.write_u16::<LittleEndian>(0xFFFF);   // NMI
    chunk.data.write_u16::<LittleEndian>(entry + 0x8000);    // RESET (execution begins here)
    chunk.data.write_u16::<LittleEndian>(0xFFFF);   // IRQ
    assert_eq!(chunk.data.len(), 0x40);
    chunk
}


pub fn link<W: Write, I: Iterator<Item=(String,LabeledChunk)>>(mut writer: W, iter: I) {
    let mut banks = Banks::new();
    for (name,block) in iter {
        banks.append(name,block);
    }
    banks.write_to(writer);
    /*
    // calculate effective PC offsets, this will be moved earlier in the pipeline later
    let size_offsets = chunks.iter().fold((LinkedHashMap::new(), 0), |mut acc, elt| {
        println!("{:?}", acc);
        acc.0.insert(&*elt.0, acc.1);
        acc.1 += elt.1.data.len();
        acc
    }).0;
    // Fill banks


    println!("{:?}",size_offsets);
    for (k,v) in chunks.iter() {
        // Cow, maybe? Or some other buffer?
        let mut c = Cursor::new(v.data.clone());
        for i in v.label_refs.iter() {
            c.seek(SeekFrom::Start(i.offset as u64 + 1)).unwrap();
            let mut val = size_offsets[&i.label] as isize;
            // TODO: why 2?
            let rel_offset = |val| val - (size_offsets[&k] + i.offset + 2) as isize;
            match i.size {
                SizeHint::Byte => c.write_u8(val as u8),
                SizeHint::Word => c.write_u16::<LittleEndian>(val as u16),
                // TODO: figure out banks
                SizeHint::Long => c.write_u24::<LittleEndian>(val as u32),
                SizeHint::RelByte => c.write_i8(rel_offset(val) as i8),
                SizeHint::RelWord => c.write_i16::<LittleEndian>(rel_offset(val) as i16),
                _ => panic!("linker error lel")
            }.unwrap();
        }
        writer.write(&c.into_inner()).unwrap();
    }*/
}
