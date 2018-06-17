// Tracks the state of local labels in a particular file.

use expression::ExprNode;

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
