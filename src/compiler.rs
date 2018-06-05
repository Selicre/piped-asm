// Stuff to help compilation
use std::collections::HashMap;
use std::mem;
use std::error::Error;

use std::rc::Rc;
use std::cell::RefCell;

use lexer::{Lexer,Span};

use parser::{Parser,Statement};
use instructions;
use instructions::Instruction as SInstruction;
use instructions::SizeHint;

use addrmodes::AddressingMode;

use byteorder::LittleEndian;
use std::io::Write;
use byteorder::WriteBytesExt;

use expression::{Expression,ExprNode,LocalLabelState};

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
    //pub label_refs: Vec<LabelRef>,
    pub pending_exprs: Vec<(usize, Expression)>,
    // todo: internal absolute refs (e.g. JMP +)
    pub bank_hint: Option<u8>
}

impl LabeledChunk {
    pub fn padding(len: usize, bank_hint: Option<u8>) -> Self {
        Self {
            data: vec![0; len],
            pending_exprs: vec![],
            bank_hint
        }
    }
    pub fn pin(&self, addr: u16) {
        // TODO: ability to pin chunks to concrete addresses
    }
    pub fn get_data(&self) -> &[u8] {
        &*self.data
    }
    /*pub fn label_refs(&self) -> &Vec<LabelRef> {
        &self.label_refs
    }*/
    pub fn size(&self) -> usize {
        self.data.len()
    }
}

#[derive(Debug,Default)]
pub struct CompilerStateInner {
    pub lls: LocalLabelState
}

#[derive(Debug,Clone,Default)]
pub struct CompilerState(Rc<RefCell<CompilerStateInner>>);

/*impl CompilerState {
    pub fn lls(&self) -> &mut LocalLabelState {
        &mut self.0.borrow_mut().lls
    }
}*/

impl ::std::ops::Deref for CompilerState {
    type Target = Rc<RefCell<CompilerStateInner>>;
    fn deref(&self) -> &Rc<RefCell<CompilerStateInner>> {
        &self.0
    }
}

pub struct Compiler {
    state: CompilerState,
    // fix?
    inner: Box<Iterator<Item=Statement>>,
    next_label: Span
}

impl Compiler {
    pub fn new(filename: &str) -> Result<Self,Box<Error>> {
        use std::io::{self,prelude::*};
        use std::fs::File;
        let state = CompilerState::default();
        let file = match filename {
            "-" => Box::new(io::stdin()) as Box<Read>,
            c => Box::new(File::open(filename)?)
        };
        let lexed = Lexer::new(filename.to_string(), file.chars().map(|c| c.unwrap()));
        let inner = Box::new(Parser::new(lexed, state.clone()).map(|c| c.unwrap()));
        Ok(Self { inner, state, next_label: Span::ident("*root".to_string(), 0, Default::default()) })
    }
    fn res_next(&mut self) -> Result<Option<(String, LabeledChunk)>,CompileError> {
        use self::Statement::*;
        let mut chunk = LabeledChunk::default();
        #[derive(Debug, Hash, PartialEq, Eq)]
        enum LabelKind {
            Pos { depth: usize, id: usize },
            Neg { depth: usize, id: usize },
            Local { stack: Vec<String> },
        }
        // label name -> offset
        let mut labels: HashMap<ExprNode, usize> = HashMap::new();
        // all the places where it should be replaced
        let mut pending_exprs: Vec<(usize, Expression)> = Vec::new();

        fn merge_labels(chunk: &mut LabeledChunk, labels: &HashMap<ExprNode, usize>, pending_exprs: Vec<(usize, Expression)>) {
            use std::io::{Cursor, Write, Seek, SeekFrom};
            //println!("{:?} => {:?}", labels, labels_used);
            let mut cursor = Cursor::new(&mut chunk.data);
            let mut linker_exprs = Vec::new();
            for (offset, mut expr) in pending_exprs.into_iter() {
                use self::ExprNode::*;
                expr.each_mut(|c| {
                    println!("{:?}\n----\n{:?}", labels, c);
                    *c = ExprNode::LabelOffset(match labels.get(c) {
                        Some(&c) => c as isize,
                        None => return
                    });
                });
                expr.reduce();
                println!("Size of {:?}: {:?}", expr.root, expr.size);
                match expr.root {
                    ExprNode::Empty => {},
                    ExprNode::Constant(c) => {
                        cursor.seek(SeekFrom::Start(offset as u64)).unwrap();
                        match expr.size {
                            SizeHint::RelByte | SizeHint::RelWord => panic!("This doesn't make any sense."),
                            SizeHint::Byte => cursor.write_u8(c as u8).unwrap(),
                            SizeHint::Word => cursor.write_u16::<LittleEndian>(c as u16).unwrap(),
                            SizeHint::Long => cursor.write_u24::<LittleEndian>(c as u32).unwrap(),
                            _ => panic!("Weird size?")
                        }
                    },
                    ExprNode::LabelOffset(c) => {
                        cursor.seek(SeekFrom::Start(offset as u64)).unwrap();
                        match expr.size {
                            SizeHint::RelByte => cursor.write_i8((c as i32 - offset as i32 - 1) as i8).unwrap(),
                            SizeHint::RelWord => cursor.write_i16::<LittleEndian>((c as i32 - offset as i32 - 1) as i16).unwrap(),
                            s => linker_exprs.push((offset, expr)),
                        }
                    },
                    _ => linker_exprs.push((offset, expr))
                }
            }
            chunk.pending_exprs = linker_exprs;
            /*for (size, addr, label) in labels_used.iter() {
                let offset = labels[label] as isize;
                // replace the address at i+1 with current
                cursor.seek(SeekFrom::Start(*addr as u64 + 1)).unwrap();
                let addr = *addr as isize;
                match size {
                    SizeHint::RelByte => cursor.write_i8((offset - addr - 2) as i8).unwrap(),
                    SizeHint::RelWord => cursor.write_i16::<LittleEndian>((offset - addr - 3) as i16).unwrap(),
                    SizeHint::Byte => {
                        cursor.write_u8(0).unwrap();
                        //println!("can't into absolute local labels yet, sorry")
                    },
                    SizeHint::Word => {
                        cursor.write_u16::<LittleEndian>(0).unwrap();
                        //println!("can't into absolute local labels yet, sorry")
                    },
                    SizeHint::Long => {
                        cursor.write_u24::<LittleEndian>(0).unwrap();
                        //println!("can't into absolute local labels yet, sorry")
                    },
                    _ => panic!("uh")
                };
            }*/
        }
        loop {
            let c = if let Some(c) = self.inner.next() { c } else {
                return match self.next_label.take() {
                    Span::Empty => Ok(None),
                    Span::Ident(c) => {
                        merge_labels(&mut chunk, &labels, pending_exprs);
                        Ok(Some((c.data, chunk)))
                    },
                    _ => unreachable!()
                };
            };

            match c {
                // Split here
                Label { name: mut name @ Span::Ident(_) } => {
                    merge_labels(&mut chunk, &labels, pending_exprs);
                    mem::swap(&mut self.next_label, &mut name);
                    let c = if let Span::Ident(c) = name { c } else { unreachable!() };
                    return Ok(Some((c.data, chunk)));
                },
                Label { name: Span::NegLabel(c) } => {
                    let c = c.data;
                    let label = self.state.borrow_mut().lls.incr_neg_id(c);
                    labels.insert(label, chunk.data.len());
                },
                Label { name: Span::PosLabel(c) } => {
                    let c = c.data;
                    let label = self.state.borrow_mut().lls.incr_pos_id(c);
                    labels.insert(label, chunk.data.len());
                },
                LocalLabel { depth, name: Span::Ident(c) } => {
                    let s = self.state.borrow_mut().lls.push_local(depth, c.data);
                    labels.insert(s, chunk.data.len());
                },
                RawData { data } => {
                    use std::io::Write;
                    chunk.data.write(&data).unwrap();
                },
                Instruction { name, size, arg } => {
                    // TODO: check for modification of compiler context (e.g. static size
                    // checking)
                    use self::ExprNode::*;
                    println!("PARSING: {:?}", name);
                    let mut const_only = true;
                    arg.expr.each(|c| match c {
                        Constant(_) => {},
                        Empty => {},
                        _ => const_only = false
                    });
                    let s = instructions::size_hint(&name.as_ident().unwrap().to_uppercase())
                        .and_then(arg.expr.size)
                        .and_then(size.0);
                    if !const_only {
                        let mut new_expr = arg.expr.clone();
                        new_expr.size = s;
                        pending_exprs.push((chunk.data.len()+1, new_expr));
                    }
                    let arg = AddressingMode::parse(arg, s).map_err(|_| { print!("wrong addressing mode {:?}", name); panic!() })?;
                    //println!("{} {:?}", name, arg);
                    SInstruction::new(name.as_ident().unwrap(), arg).write_to(&mut chunk.data).unwrap();
                },
                Attributes { .. } => {
                    //println!("(some attributes, todo: change compiler context)");
                }
                c => {
                    panic!("unknown statement {:?}", c);
                }
            }
        }
    }
}

impl Iterator for Compiler {
    type Item = Result<(String, LabeledChunk),CompileError>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.res_next() {
            Ok(Some(c)) => Some(Ok(c)),
            Ok(None) => None,
            Err(e) => Some(Err(e))
        }
    }
}
