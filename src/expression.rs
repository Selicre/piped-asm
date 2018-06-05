// Expression evaluator
// Supposed to support all sorts of complex math, functions, and even inline lua scripts.
use lexer::Span;
use std::fmt;
use instructions::SizeHint;

#[derive(Debug,Default)]
pub struct LocalLabelState {
    pos_labels: Vec<usize>,
    neg_labels: Vec<usize>,
    local_labels: Vec<String>
}

impl LocalLabelState {
    pub fn get_pos_id(&mut self, depth: usize) -> ExprNode {
        if self.pos_labels.len() < depth+1 { self.pos_labels.resize(depth+1, 0); }
        // always return the one in front, not behind
        let id = self.pos_labels[depth] + 1;
        ExprNode::PosLabel { depth, id }
    }
    pub fn incr_pos_id(&mut self, depth: usize) -> ExprNode {
        if self.pos_labels.len() < depth+1 { self.pos_labels.resize(depth+1, 0); }
        self.pos_labels[depth] += 1;
        let id = self.pos_labels[depth];
        ExprNode::PosLabel { depth, id }
    }
    pub fn get_neg_id(&mut self, depth: usize) -> ExprNode {
        if self.neg_labels.len() < depth+1 { self.neg_labels.resize(depth+1, 0); }
        let id = self.neg_labels[depth];
        ExprNode::NegLabel { depth, id }
    }
    pub fn incr_neg_id(&mut self, depth: usize) -> ExprNode {
        if self.neg_labels.len() < depth+1 { self.neg_labels.resize(depth+1, 0); }
        self.neg_labels[depth] += 1;
        let id = self.neg_labels[depth];
        ExprNode::NegLabel { depth, id }
    }
    pub fn get_local(&self, depth: usize, new: String) -> ExprNode {
        let mut stack = self.local_labels[..depth-1].to_vec();
        stack.push(new);
        ExprNode::LocalLabel { stack }
    }
    pub fn push_local(&mut self, depth: usize, s: String) -> ExprNode {
        self.local_labels.resize(depth, "(anonymous)".to_string());
        self.local_labels.push(s);
        let stack = self.local_labels.to_vec();
        ExprNode::LocalLabel { stack }
    }
}

#[derive(Debug,Clone)]
pub struct Expression {
    pub root: ExprNode,
    pub size: SizeHint
}

// TODO: use SpanData?

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprNode {
    Empty,                  // special case
    Constant(i32),          // regular number
    LabelOffset(isize),     // offset from start of label
    LocalLabel { stack: Vec<String> },
    PosLabel { depth: usize, id: usize },
    NegLabel { depth: usize, id: usize },
    Label(String),          // actual label
    Str(String),            // string literal
    BinOp {
        op: BinOp,
        lhs: Box<ExprNode>,
        rhs: Box<ExprNode>,
    },
    UnOp(UnOp, Box<ExprNode>)
}

impl fmt::Display for ExprNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ExprNode::*;
        match self {
            Empty => write!(f, "empty"),
            Constant(c) => write!(f, "{}", c),
            LabelOffset(c) => write!(f, "Label offset {}", c),
            Label(ref s) => write!(f, "{}", s),
            PosLabel { depth, .. } => write!(f, "+({})", depth),
            NegLabel { depth, .. } => write!(f, "-({})", depth),
            BinOp { op, lhs, rhs } => {
                write!(f, "{:?}({}, {})", op, lhs, rhs)
            },
            UnOp(op, c) => {
                write!(f, "{:?}({})", op, c)
            },
            _ => write!(f, "something else")
        }
    }
}

#[derive(Debug,Clone,Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    // Bitwise
    And,
     Or,
    Xor,
    Lsr, // shift right
    Asl, // shift left
}

impl BinOp {
    fn precedence(&self) -> usize {
        use self::BinOp::*;
        match self {
            And => 1,
            Or  => 1,
            Xor => 1,
            Lsr => 2,
            Asl => 2,
            Add => 3,
            Sub => 3,
            Mul => 4,
            Div => 4,
        }
    }
    fn execute(&self, lhs: i32, rhs: i32) -> i32 {
        use self::BinOp::*;
        match self {
            And => lhs & rhs,
            Or  => lhs | rhs,
            Xor => lhs ^ rhs,
            Lsr => lhs >> rhs,
            Asl => lhs << rhs,
            Add => lhs + rhs,
            Sub => lhs - rhs,
            Mul => lhs * rhs,
            Div => lhs / rhs,   // todo: check overflow for all of these
        }
    }
    fn exec_node(&self, lhs: &ExprNode, rhs: &ExprNode) -> Option<ExprNode> {
        Some(match (lhs,rhs) {
            (ExprNode::Constant(l), ExprNode::Constant(r)) =>
                ExprNode::Constant(self.execute(*l,*r)),
            (ExprNode::LabelOffset(l), ExprNode::LabelOffset(r)) if *self == BinOp::Sub =>
                ExprNode::Constant(self.execute(*l as i32, *r as i32)),
            (ExprNode::LabelOffset(l), ExprNode::Constant(r)) if *self == BinOp::Add || *self == BinOp::Sub =>
                ExprNode::LabelOffset(self.execute(*l as i32,*r) as isize),
            (lhs,rhs) => return None
        })
    }
    fn parse(list: &mut SpanList) -> Option<Self> {
        use lexer::Span::*;
        use self::BinOp::*;
        Some(match list.next()? {
            Symbol('&',_) => And,
            Symbol('|',_) => Or,
            Symbol('^',_) => Xor,
            Symbol('<',_) => if list.peek(0).map(|c| c.is_symbol('<')).unwrap_or_default() {
                list.next();
                Asl
            } else {
                panic!("no less-than yet")
            },
            Symbol('>',_) => if list.peek(0).map(|c| c.is_symbol('<')).unwrap_or_default() {
                list.next();
                Lsr
            } else {
                panic!("no greater-than yet")
            },
            PosLabel(ref c) if c.data == 0 => Add,
            NegLabel(ref c) if c.data == 0 => Sub,
            Symbol('*',_) => Mul,
            Symbol('/',_) => Div,
            ref c => panic!("{:?}", c) //return None
        })
    }
}

#[derive(Debug,Clone,Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    // Unary
    Unm,
    Not
}

impl UnOp {
    fn parse(list: &mut SpanList) -> Option<Self> {
        use lexer::Span::*;
        use self::UnOp::*;
        Some(match list.next()? {
            Symbol('~',_) => Not,
            NegLabel(ref c) if c.data == 0 => Unm,
            _ => None?
        })
    }
    fn execute(&self, c: i32) -> i32 {
        use self::UnOp::*;
        match self {
            Unm => -c,
            Not => !c
        }
    }
    fn exec_node(&self, c: &ExprNode) -> Option<ExprNode> {
        if let ExprNode::Constant(l) = c {
            return Some(ExprNode::Constant(self.execute(*l)))
        }
        None
    }
}

struct ParserState<'a> {
    size_hint: SizeHint,
    lls: &'a mut LocalLabelState
}

fn parse_unops(expr: &mut SpanList, state: &mut ParserState) -> Result<ExprNode,ExprError> {
    if let Some(op) = expr.peek_unop() {
        expr.next_unop();
        Ok(ExprNode::new_un_op(op, parse_unops(expr, state)?))
    } else {
        let ex = expr.next_node(state);
        ex
    }
}

fn parse_expr(expr: &mut SpanList, prev: Option<ExprNode>, depth: usize, state: &mut ParserState) -> Result<ExprNode,ExprError> {
    let term = match prev {
        Some(c) => c,
        None => parse_unops(expr, state)?
    };
    // TODO: functions
    let op = match expr.peek_op() {
        Some(op) => op,
        None => return Ok(term)
    };
    if op.precedence() > depth {
        expr.next_op();
        let out = ExprNode::new_bin_op(op, term, parse_expr(expr, None, op.precedence(), state)?);
        parse_expr(expr, Some(out), 0, state)
    } else {
        Ok(term)
    }
}
/*
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test1() {
        use lexer::Lexer;
        let expr = Lexer::new("(test)".into(), ".- - -.-".chars()).collect::<Vec<_>>();
        println!("{}", expr.iter().map(|c| format!("{}",c)).collect::<Vec<_>>().join(", "));
        let out = ExprNode::parse(&expr[..expr.len()-1]).unwrap().0;
        println!("OUTPUT: {}", out);
        let mut d = Vec::new();
        out.each(|c| d.push(c.clone()));
        println!("{:?}",d);
    }
}*/
impl Expression {
    pub fn parse(expr: &[Span], lls: &mut LocalLabelState) -> Result<Self,ExprError> {
        let mut state = ParserState {
            size_hint: SizeHint::default(),
            lls
        };
        let (root, size) = ExprNode::parse(expr, &mut state)?;
        Ok(Self { root, size })
    }
    pub fn empty() -> Self {
        Self { root: ExprNode::Empty, size: SizeHint::default() }
    }
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.root)
    }
}
impl ::std::ops::Deref for Expression {
    type Target = ExprNode;
    fn deref(&self) -> &ExprNode {
        &self.root
    }
}
impl ::std::ops::DerefMut for Expression {
    fn deref_mut(&mut self) -> &mut ExprNode {
        &mut self.root
    }
}
impl ExprNode {
    // todo: base this on a peekable iterator?
    fn parse(expr: &[Span], state: &mut ParserState) -> Result<(Self, SizeHint),ExprError> {
        if expr.len() == 1 {
            match expr[0] {
                Span::PosLabel(ref c) => return Ok((state.lls.get_pos_id(c.data), SizeHint::default())),
                Span::NegLabel(ref c) => return Ok((state.lls.get_neg_id(c.data), SizeHint::default())),
                _ => {}
            }
        }
        let mut list = SpanList::new(expr);
        let expr = parse_expr(&mut list, None, 0, state)?;
        Ok((expr, state.size_hint))
    }
    pub fn reduce(&mut self) {
        use self::ExprNode::*;
        match self {
            BinOp { op, lhs, rhs } => {
                if let Some(c) = op.exec_node(&**lhs, &**rhs) {
                    *self = c;
                }
            },
            _ => {}
        }
    }
    pub fn with_size(self, size: SizeHint) -> Expression {
        Expression { root: self, size }
    }
    pub fn each_mut(&mut self, mut f: impl FnMut(&mut ExprNode)) {
        fn a(e: &mut ExprNode, f: &mut impl FnMut(&mut ExprNode)) {
            match e {
                ExprNode::BinOp { op: _, lhs, rhs } => {
                    a(lhs, f); a(rhs, f);
                },
                ExprNode::UnOp(_, c) => a(c, f),
                mut c => f(&mut c)
            }
        }
        a(self, &mut f)
    }
    pub fn each(&self, mut f: impl FnMut(&ExprNode)) {
        fn a(e: &ExprNode, f: &mut impl FnMut(&ExprNode)) {
            match e {
                ExprNode::BinOp { op: _, lhs, rhs } => {
                    a(lhs,f); a(rhs, f);
                },
                ExprNode::UnOp(_, c) => a(c, f),
                mut c => f(&mut c)
            }
        }
        a(self, &mut f)
    }
    fn new_bin_op(op: BinOp, lhs: Self, rhs: Self) -> Self {
        op.exec_node(&lhs, &rhs).unwrap_or_else(|| {
            ExprNode::BinOp { op, lhs: Box::new(lhs), rhs: Box::new(rhs) }
        })
    }
    fn new_un_op(op: UnOp, lhs: Self) -> Self {
        op.exec_node(&lhs).unwrap_or_else(|| {
            ExprNode::UnOp(op, Box::new(lhs))
        })
    }
    fn from_span(c: &mut SpanList, state: &mut ParserState) -> Result<Self,ExprError> {
        use self::Span::*;
        let next = c.next()?;
        Ok(match next {
            Ident(c) => ExprNode::Label(c.data.clone()),
            Number(d) | Byte(d) | Word(d) | Long(d) => {
                state.size_hint = state.size_hint.max(next.size());
                ExprNode::Constant(d.data)
            },
            Symbol('(',_) => {
                let prev = c.cur();
                let mut depth = 1;
                // todo: maybe this can be done in a more straightforward way
                while depth > 0 {
                    let ch = c.next().ok_or(ExprError::EOF)?;
                    if ch.is_symbol('(') { depth += 1; }
                    if ch.is_symbol(')') { depth -= 1; }
                }
                let (expr, size) = Self::parse(&c[prev..c.cur()-1], state)?;
                expr
            },
            Symbol('.',_) => {
                let mut depth = 1;
                while c.peek(0).ok_or(ExprError::EOF)?.is_symbol('.') { depth += 1; c.next(); }
                match c.next()? {
                    PosLabel(d) => state.lls.get_pos_id(d.data),
                    NegLabel(d) => state.lls.get_neg_id(d.data),
                    Ident(d) => state.lls.get_local(depth, d.data.clone()),
                    _ => Err(ExprError::Other)?
                }
            },
            _ => Err(ExprError::Other)?
        })
    }
}

#[derive(Debug)]
pub enum ExprError {
    Other,
    EOF
}

use std::option::NoneError;
impl From<NoneError> for ExprError {
    fn from(_: NoneError) -> Self {
        ExprError::EOF
    }
}


struct SpanList<'a> {
    cur: usize,
    src: &'a [Span]
}
impl<'a> SpanList<'a> {
    fn new(src: &'a [Span]) -> Self {
        Self { src, cur: 0 }
    }
    fn cur(&self) -> usize {
        self.cur
    }
    fn set_pos(&mut self, pos: usize) {
        self.cur = pos
    }
    fn peek(&mut self, index: isize) -> Option<&'a Span> {
        self.src.get((self.cur as isize + index) as usize)
    }
    fn next(&mut self) -> Option<&'a Span> {
        self.src.get(self.cur).map(|c| {
            self.cur += 1;
            c
        })
    }
    fn next_node(&mut self, state: &mut ParserState) -> Result<ExprNode,ExprError> {
        while self.peek(0).map(|c| c.is_whitespace()).unwrap_or_default() { self.next(); }
        ExprNode::from_span(self, state)
    }
    fn next_op(&mut self) -> Option<BinOp> {
        while self.peek(0).map(|c| c.is_whitespace()).unwrap_or_default() { self.next(); }
        BinOp::parse(self)
    }
    fn peek_op(&mut self) -> Option<BinOp> {
        let cur = self.cur;
        let c = self.next_op();
        self.cur = cur;
        c
    }
    fn next_unop(&mut self) -> Option<UnOp> {
        while self.peek(0).map(|c| c.is_whitespace()).unwrap_or_default() { self.next(); }
        UnOp::parse(self)
    }
    fn peek_unop(&mut self) -> Option<UnOp> {
        let cur = self.cur;
        let c = self.next_unop();
        self.cur = cur;
        c
    }
    fn rest(&self) -> Self {
        Self::new(&self.src[self.cur..])
    }
}
impl<'a> ::std::ops::Deref for SpanList<'a> {
    type Target = [Span];
    fn deref(&self) -> &[Span] {
        &self.src
    }
}




