// Expression evaluator
// Supposed to support all sorts of complex math, functions, and even inline lua scripts.
use lexer::Span;
use std::fmt;
use instructions::SizeHint;
pub use lls::LocalLabelState;
use n_peek::{NPeekable,NPeekableHandle};


#[derive(Debug,Clone)]
pub struct Expression {
    pub root: ExprNode,
    pub size: SizeHint,
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
            _ => return None
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

fn parse_unops<I: Iterator<Item=Span>>(expr: &mut SpanView<I>, state: &mut ParserState) -> Result<ExprNode,ExprError> {
    if let Some(op) = expr.peek_unop() {
        expr.next_unop();
        Ok(ExprNode::new_un_op(op, parse_unops(expr, state)?))
    } else {
        let ex = expr.next_node(state);
        ex
    }
}

fn parse_expr<I: Iterator<Item=Span>>(expr: &mut SpanView<I>, prev: Option<ExprNode>, depth: usize, state: &mut ParserState) -> Result<ExprNode,ExprError> {
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


impl Expression {
    pub fn parse(expr: &mut NPeekable<impl Iterator<Item=Span>>, lls: &mut LocalLabelState) -> Result<Self,ExprError> {
        let mut state = ParserState {
            size_hint: SizeHint::default(),
            lls
        };
        if expr.peek(1).is_none() {
            match expr.peek(0)? {
                Span::PosLabel(ref c) => return Ok(Self { root: state.lls.get_pos_id(c.data), size: SizeHint::default() }),
                Span::NegLabel(ref c) => return Ok(Self { root: state.lls.get_neg_id(c.data), size: SizeHint::default() }),
                _ => {}
            }
        }
        let mut view = SpanView::new(expr);
        let (root, size) = ExprNode::parse(&mut view, &mut state)?;
        Ok(Self { root, size })
    }
    pub fn with_size(&mut self, size: SizeHint) {
        self.size = self.size.and_then(size);
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
    pub fn is_const(&self) -> Option<i32> {
        if let ExprNode::Constant(c) = self { Some(*c) } else { None }
    }
    fn parse<I: Iterator<Item=Span>>(expr: &mut SpanView<I>, state: &mut ParserState) -> Result<(Self,SizeHint),ExprError> {
        let expr = parse_expr(expr, None, 0, state)?;
        Ok((expr, state.size_hint))
    }
    pub fn reduce(&mut self) -> bool {
        use self::ExprNode::*;
        let mut result = false;
        match self {
            BinOp { ref op, ref mut lhs, ref mut rhs } => {
                result |= lhs.reduce();
                result |= rhs.reduce();
                if let Some(c) = op.exec_node(&lhs, &rhs) {
                    *self = c;
                    result = true;
                }
            },
            UnOp(ref op,ref mut c) => {
                result |= c.reduce();
                if let Some(c) = op.exec_node(&c) {
                    *self = c;
                    result = true;
                }
            }
            _ => {}
        }
        result
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
}

#[derive(Debug)]
pub enum ExprError {
    InvalidOperand(Span),
    InvalidAfterDot(Span),
    Other,
    EOF(Span)
}

use std::option::NoneError;
impl From<NoneError> for ExprError {
    fn from(_: NoneError) -> Self {
        ExprError::EOF(Span::Empty)
    }
}

fn erase_type(i: impl Iterator) -> impl Iterator {
    i
}

// Helper wrapper struct.
struct SpanView<'a, I: Iterator + 'a> {
    iter: &'a mut NPeekable<I>
}
impl<'a, I: Iterator<Item=Span> + 'a> SpanView<'a, I> {
    fn new(iter: &'a mut NPeekable<I>) -> Self {
        Self { iter }
    }
    fn next_node(&mut self, state: &mut ParserState) -> Result<ExprNode,ExprError> {
        use self::Span::*;
        let c = &mut self.iter;
        let next = c.next()?;
        let size = next.size();
        Ok(match next {
            Ident(c) => ExprNode::Label(c.data.clone()),
            Number(d) | Byte(d) | Word(d) | Long(d) => {
                state.size_hint = state.size_hint.max(size);
                ExprNode::Constant(d.data)
            },
            String(c) => ExprNode::Str(c.data.clone()),
            Symbol('(',_) => {
                // TODO: make this mess a bit better
                let iter = Box::new(self.by_ref().take_while(|c| !c.is_symbol(')'))) as Box<Iterator<Item=_>>;
                let iter = &mut NPeekable::new(iter);
                let mut view = SpanView::new(iter);
                let (expr, _size) = ExprNode::parse(&mut view, state)?;
                expr
            },
            Symbol('.',_) => {
                let mut depth = 1;
                while c.peek(0)?.is_symbol('.') { depth += 1; c.next(); }
                match c.next()? {
                    PosLabel(d) => state.lls.get_pos_id(d.data),
                    NegLabel(d) => state.lls.get_neg_id(d.data),
                    Ident(d) => state.lls.get_local(depth, d.data.clone()),
                    c => Err(ExprError::InvalidAfterDot(c.clone()))?
                }
            },
            c => Err(ExprError::InvalidOperand(c.clone()))?
        })
    }
    fn next_op(&mut self) -> Option<BinOp> {
        let mut h = self.handle();
        let op = Self::peek_op_inner(&mut h);
        h.buffer();
        op
    }
    fn peek_op(&mut self) -> Option<BinOp> {
        Self::peek_op_inner(&mut self.handle())
    }
    fn peek_op_inner(handle: &mut NPeekableHandle<I>) -> Option<BinOp> {
        use lexer::Span::*;
        use self::BinOp::*;
        Some(match handle.next()? {
            Symbol('&',_) => And,
            Symbol('|',_) => Or,
            Symbol('^',_) => Xor,
            Symbol('<',_) => if handle.peek(0).map(|c| c.is_symbol('<')).unwrap_or_default() {
                handle.next();
                Asl
            } else {
                panic!("no less-than yet")
            },
            Symbol('>',_) => if handle.peek(0).map(|c| c.is_symbol('>')).unwrap_or_default() {
                handle.next();
                Lsr
            } else {
                panic!("no greater-than yet")
            },
            PosLabel(ref c) if c.data == 0 => Add,
            NegLabel(ref c) if c.data == 0 => Sub,
            Symbol('*',_) => Mul,
            Symbol('/',_) => Div,
            ref c => panic!("No such operand: {:?}", c) //return None
        })
    }
    fn next_unop(&mut self) -> Option<UnOp> {
        let op = self.peek_unop();
        self.iter.next();
        op
    }
    fn peek_unop(&mut self) -> Option<UnOp> {
        use lexer::Span::*;
        use self::UnOp::*;
        Some(match self.iter.peek(0)? {
            Symbol('~',_) => Not,
            NegLabel(ref c) if c.data == 0 => Unm,
            _ => None?
        })
    }
}

impl<'a, I: Iterator + 'a> ::std::ops::Deref for SpanView<'a,I> {
    type Target = NPeekable<I>;
    fn deref(&self) -> &Self::Target {
        self.iter
    }
}
impl<'a, I: Iterator + 'a> ::std::ops::DerefMut for SpanView<'a,I> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.iter
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Lexer;
    fn process(c: &str, res: &ExprNode) {
        let lexed = Lexer::new("<test>".to_string(), c.chars())
            .filter(|c| !c.is_whitespace());
        let mut iter = NPeekable::new(lexed);
        let expr = Expression::parse(&mut iter, &mut Default::default()).unwrap();
        assert_eq!(&expr.root, res);
    }
    #[test]
    fn basic() {
        use self::ExprNode::*;
        let cases = [
            ("2 + 4",       Constant(6)),
            ("-2 + 4",      Constant(2)),
            ("2 + -4",      Constant(-2)),
            ("-2 + -4",     Constant(-6)),
            ("2 << 2",      Constant(8)),
            ("2 * (2 + 2)", Constant(8)),
            ("(3 + 2) * 2", Constant(10)),
            ("(3+2*3)*2",   Constant(18)),
            //("-",            NegLabel { depth: 0, id: 0 }),
        ];
        for (inp, out) in cases.into_iter() {
            process(inp, out);
        }
    }
}
