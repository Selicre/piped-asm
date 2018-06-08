// Stuff to help compilation
use std::collections::HashMap;
use std::mem;
use std::error::Error;

use std::rc::Rc;
use std::cell::RefCell;

use lexer::{Lexer,Span,SpanData};

use parser::{Parser,Statement};
use instructions;
use instructions::Instruction as SInstruction;
use instructions::SizeHint;

use addrmodes::AddressingMode;

use byteorder::LittleEndian;
use std::io::Write;
use byteorder::WriteBytesExt;

use expression::{Expression,ExprNode,LocalLabelState};

use attributes::Attribute;

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
    pub pending_exprs: Vec<(usize, Expression)>,
    pub attrs: Vec<Attribute>,
    pub diverging: bool,
    pub bank_hint: Option<u8>
}

impl LabeledChunk {
    pub fn padding(len: usize, bank_hint: Option<u8>) -> Self {
        Self {
            data: vec![0; len],
            diverging: true,
            attrs: Vec::new(),
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
    next_label: Option<SpanData<String>>,
    next_attrs: Vec<Attribute>
}

impl Compiler {
    pub fn new(filename: &str) -> Result<Self,Box<Error>> {
        use std::io::{self,prelude::*,BufReader};
        use std::fs::File;
        let state = CompilerState::default();
        let file = match filename {
            "-" => Box::new(io::stdin()) as Box<Read>,
            c => Box::new(File::open(filename)?)
        };
        let file = BufReader::new(file);
        let lexed = Lexer::new(filename.to_string(), file.chars().map(|c| c.unwrap()));
        let inner = Box::new(Parser::new(lexed, state.clone()).map(|c| c.unwrap()));
        Ok(Self { inner, state, next_attrs: Vec::new(), next_label: Some(SpanData::create("*root".to_string())) })
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
            let mut cursor = Cursor::new(&mut chunk.data);
            let mut linker_exprs = Vec::new();
            for (offset, mut expr) in pending_exprs.into_iter() {
                use self::ExprNode::*;
                expr.each_mut(|c| {
                    *c = ExprNode::LabelOffset(match labels.get(c) {
                        Some(&c) => c as isize,
                        None => return
                    });
                });
                expr.reduce();
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
        }
        loop {
            let c = if let Some(c) = self.inner.next() { c } else {
                return match self.next_label.take() {
                    None => Ok(None),
                    Some(c) => {
                        merge_labels(&mut chunk, &labels, pending_exprs);
                        Ok(Some((c.data, chunk)))
                    }
                }
            };

            match c {
                // Split here
                Label { name: Span::Ident(mut name), mut attrs } => {
                    merge_labels(&mut chunk, &labels, pending_exprs);
                    mem::swap(self.next_label.as_mut().unwrap(), &mut name);
                    mem::swap(&mut self.next_attrs, &mut attrs);
                    for i in &attrs { match i {
                        Attribute::Bank(c) => chunk.bank_hint = Some(*c),
                        _ => {}
                    } }
                    chunk.attrs = attrs;
                    return Ok(Some((name.data, chunk)));
                },
                // TODO: move this to the parser? Maybe? It's a bit split rn
                Label { name: Span::NegLabel(c), .. } => {
                    //chunk.diverging = false; // doesn't actually make it divergent
                    let c = c.data;
                    let label = self.state.borrow_mut().lls.incr_neg_id(c);
                    labels.insert(label, chunk.data.len());
                },
                Label { name: Span::PosLabel(c), .. } => {
                    chunk.diverging = false;
                    let c = c.data;
                    let label = self.state.borrow_mut().lls.incr_pos_id(c);
                    labels.insert(label, chunk.data.len());
                },
                LocalLabel { depth, name: Span::Ident(c) } => {
                    chunk.diverging = false;
                    let s = self.state.borrow_mut().lls.push_local(depth, c.data);
                    labels.insert(s, chunk.data.len());
                },
                RawData { data, pending_exprs: p } => {
                    // Executing raw data is not advisable.
                    chunk.diverging = true;
                    use std::io::Write;
                    let len = chunk.data.len();
                    pending_exprs.extend(p.into_iter().map(|(off, expr)| (len+off, expr)));
                    chunk.data.write(&data).unwrap();
                },
                Instruction { attrs, name, size, arg } => {
                    // TODO: check for modification of compiler context (e.g. static size
                    // checking)
                    use self::ExprNode::*;
                    //println!("PARSING: {:?}", name);
                    let mut const_only = true;
                    arg.expr.each(|c| match c {
                        Constant(_) => {},
                        Empty => {},
                        _ => const_only = false
                    });
                    let s = instructions::size_hint(&name.as_ident().unwrap().to_uppercase());
                    // if implicit size (INC/DEC), then don't add it
                    // TODO: fix inconsistency?
                    const_only |= s == SizeHint::Implicit;
                    let s = s.and_then(arg.expr.size)
                        .and_then(size.0);
                    if !const_only {
                        let mut new_expr = arg.expr.clone();
                        new_expr.size = s;
                        pending_exprs.push((chunk.data.len()+1, new_expr));
                    }
                    let arg = AddressingMode::parse(arg, s).map_err(|_| { print!("wrong addressing mode {:?}", name); panic!() })?;
                    let instr = SInstruction::new(name.as_ident().unwrap(), arg);
                    if instr.is_diverging() { chunk.diverging = true; }
                    instr.write_to(&mut chunk.data).unwrap();
                },
                Attributes { ref attrs } => {
                    // todo
                },
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
