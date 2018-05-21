// Stuff to help compilation
use std::collections::HashMap;
use std::mem;

use lexer::Span;

use parser::{Statement};
use instructions;
use instructions::Instruction as SInstruction;
use instructions::SizeHint;

use addrmodes::AddressingMode;

use byteorder::LittleEndian;
use std::io::Write;
use byteorder::WriteBytesExt;


struct CompilerContext {
    local: CtxInner,
    global: CtxInner
}

#[derive(Default)]
struct CtxInner {
    a_size: Option<bool>,    // false: 8-bit, true: 16-bit
    xy_size: Option<bool>,
}
#[derive(Debug)]
pub enum CompileError {
    
}
/*
// Intermediate struct to build a LabeledChunk.
struct LabeledChunkBuilder {
    data: Vec<u8>,
    // [n] => depth 1-n, Stores the offset of negative label to jump to it.
    neg_labels: Vec<usize>,
    // [n] => depth n-1, Stores the offset of every instruction to overwrite with
    pos_labels: Vec<Vec<usize>>,
    local_labels: Vec<usize>,
}


impl LabeledChunkBuilder {
    pub fn feed(self, stmt: Statement) -> Result<Self,CompileError> {
        match stmt {
            Label {} => panic!("compiler broke: can't feed a Label to a LabeledChunkBuilder")
        }
    }
}*/
#[derive(Default, Debug)]
pub struct LabelRef {
    pub offset: usize,
    pub size: SizeHint,
    pub label: String
}

// TODO: (de)serialize a hashmap of this
#[derive(Default, Debug)]
pub struct LabeledChunk {
    pub data: Vec<u8>,
    pub label_refs: Vec<LabelRef>,
    // todo: internal absolute refs (e.g. JMP +)
    pub bank_hint: Option<u8>
}

impl LabeledChunk {
    pub fn padding(len: usize, bank_hint: Option<u8>) -> Self {
        Self {
            data: vec![0; len],
            label_refs: vec![],
            bank_hint
        }
    }
    pub fn pin(&self, addr: u16) {
        // TODO: ability to pin chunks to concrete addresses
    }
    pub fn get_data(&self) -> &[u8] {
        &*self.data
    }
    pub fn label_refs(&self) -> &Vec<LabelRef> {
        &self.label_refs
    }
    pub fn size(&self) -> usize {
        self.data.len()
    }
}

pub struct Compiler<I> {
    inner: I,
    next_label: Span
}

impl<I: Iterator<Item=Statement>> Compiler<I> {
    pub fn new(inner: I) -> Self {
        Self { inner, next_label: Span::ident("*root".to_string(), 0, Default::default()) }
    }
    fn res_next(&mut self) -> Result<Option<(String, LabeledChunk)>,CompileError> {
        use self::Statement::*;
        let mut chunk = LabeledChunk::default();
        #[derive(Hash, PartialEq, Eq)]
        enum LabelKind {
            Pos { depth: usize, id: usize },
            Neg { depth: usize, id: usize },
            Local { stack: Vec<String> },
        }
        // You can't jump to an anonymous label across a non-local label because that prevents
        // reordering
        // An array of IDs for each level
        let mut pos_labels = Vec::new();
        // For negative labels, id 0 is invalid
        let mut neg_labels = Vec::new();
        // Current local label stack.
        let mut label_stack = Vec::new();
        // label name -> offset
        let mut labels: HashMap<LabelKind, usize> = HashMap::new();
        // all the places where it should be replaced
        let mut labels_used: Vec<(SizeHint, usize, LabelKind)> = Vec::new();
        loop {
            let c = if let Some(c) = self.inner.next() { c } else {
                return match self.next_label.take() {
                    Span::Empty => Ok(None),
                    Span::Ident(c) => Ok(Some((c.data, chunk))),
                    _ => unreachable!()
                };
            };

            match c {
                // Split here
                Label { name: mut name @ Span::Ident(_) } => {
                    {
                        use std::io::{Cursor, Write, Seek, SeekFrom};
                        let mut cursor = Cursor::new(&mut chunk.data);
                        for (size, addr, label) in labels_used.iter() {
                            let offset = labels[label] as isize;
                            // replace the address at i+1 with current
                            cursor.seek(SeekFrom::Start(*addr as u64 + 1)).unwrap();
                            let addr = *addr as isize;
                            match size {
                                SizeHint::RelByte => cursor.write_i8((offset - addr - 2) as i8).unwrap(),
                                SizeHint::RelWord => cursor.write_i16::<LittleEndian>((offset - addr - 3) as i16).unwrap(),
                                _ => panic!("can't into absolute local labels yet, sorry")
                            };
                        }
                    }
                    mem::swap(&mut self.next_label, &mut name);
                    let c = if let Span::Ident(c) = name { c } else { unreachable!() };
                    return Ok(Some((c.data, chunk)));
                },
                Label { name: Span::NegLabel(c) } => {
                    let c = c.data;
                    if neg_labels.len() < c+1 { neg_labels.resize(c+1, 0); }
                    neg_labels[c] += 1;
                    labels.insert(LabelKind::Neg { depth: c, id: neg_labels[c] }, chunk.data.len());
                },
                Label { name: Span::PosLabel(c) } => {
                    let c = c.data;
                    if pos_labels.len() < c+1 { pos_labels.resize(c+1, 0); }
                    neg_labels[c] += 1;
                    labels.insert(LabelKind::Pos { depth: c, id: neg_labels[c] }, chunk.data.len());
                },
                LocalLabel { depth, name: Span::Ident(c) } => {
                    // trim the stack
                    label_stack.resize(depth, "(anonymous)".to_string());
                    label_stack.push(c.data);
                    labels.insert(LabelKind::Local { stack: label_stack.clone() }, chunk.data.len());
                },
                RawData { data } => {
                    use std::io::Write;
                    chunk.data.write(&data).unwrap();
                },
                Instruction { name, size, mut arg } => {
                    // TODO: check for modification of compiler context (e.g. static size
                    // checking)
                    let mut s = SizeHint::Unspecified;
                    match arg.span {
                        Span::Ident(c) => {
                            // also cover other variants
                            let d = c.replace(0);
                            s = s.and_then(instructions::size_hint(&name.as_ident().unwrap().to_uppercase()));
                            s = s.and_then(size.0);
                            chunk.label_refs.push(LabelRef { offset: chunk.data.len(), size: s, label: c.data });
                            arg.span = Span::Number(d);
                        },
                        Span::NegLabel(c) => {
                            let depth = c.data;
                            let d = c.replace(0);
                            if neg_labels.len() < depth+1 { neg_labels.resize(depth+1, 0); }
                            s = s.and_then(instructions::size_hint(&name.as_ident().unwrap().to_uppercase()));
                            s = s.and_then(size.0);
                            labels_used.push((s, chunk.data.len(), LabelKind::Neg { depth, id: neg_labels[depth] }));
                            arg.span = Span::Number(d);
                        },
                        Span::PosLabel(c) => {
                            let depth = c.data;
                            let d = c.replace(0);
                            s = s.and_then(instructions::size_hint(&name.as_ident().unwrap().to_uppercase()));
                            s = s.and_then(size.0);
                            labels_used.push((s, chunk.data.len(), LabelKind::Pos { depth, id: pos_labels[depth] }));
                            arg.span = Span::Number(d);
                        },
                        // This is a local label for now.
                        ref mut c @ Span::Successive(_) => {
                            let mut d = None;
                            if let Some((depth, label)) = c.is_local_label() {
                                let l = if let Span::Ident(l) = label { l } else { panic!() };
                                let label_name = l.data.clone();
                                d = Some(l.replace(0));
                                s = s.and_then(instructions::size_hint(&name.as_ident().unwrap().to_uppercase()));
                                s = s.and_then(size.0);
                                // todo: range checks
                                let mut stack = label_stack[..depth].to_vec();
                                stack.push(label_name);
                                labels_used.push((s, chunk.data.len(), LabelKind::Local { stack }));
                            }
                            *c = Span::Number(d.unwrap());
                        },
                        _ => s = s.and_then(size.0),
                    }
                    let arg = AddressingMode::parse(arg, s).map_err(|_| { print!("wrong addressing mode {:?}", name); panic!() })?;
                    SInstruction::new(name.as_ident().unwrap(), arg).write_to(&mut chunk.data).unwrap();
                },
                Attributes { .. } => {
                    //println!("(some attributes, todo: change compiler context)");
                }
                c => {
                    //panic!("i don't wanna eat this")
                    println!("SKIPPING OVER SOME SHIT");
                    println!("{:?}", c);
                }
            }
        }
    }
}

impl<I: Iterator<Item=Statement>> Iterator for Compiler<I> {
    type Item = Result<(String, LabeledChunk),CompileError>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.res_next() {
            Ok(Some(c)) => Some(Ok(c)),
            Ok(None) => None,
            Err(e) => Some(Err(e))
        }
    }
}
