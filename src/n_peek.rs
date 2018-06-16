// Iterator tool, like Peekable, but any amount of peeks
// Internally, uses a Vec instead of a VecDeque as it is optimized to return its buffer.

use std::collections::VecDeque;
use std::mem;

pub struct NPeekable<I: Iterator> {
    queue: Vec<I::Item>,
    iter: I
}

impl<I: Iterator> Iterator for NPeekable<I> {
    type Item = I::Item;
    fn next(&mut self) -> Option<I::Item> {
        if self.queue.len() > 0 {
            Some(self.queue.remove(0))
        } else {
            self.iter.next()
        }
    }
}

impl<I: Iterator> NPeekable<I> {
    // c.peek(0) == &c.next()
    pub fn peek(&mut self, offset: usize) -> Option<&I::Item> {
        while offset >= self.queue.len() {
            let next = self.iter.next()?;
            self.queue.push(next);
        }
        Some(&self.queue[offset])
    }
    pub fn new(iter: I) -> Self {
        Self { iter, queue: Vec::new() }
    }
    pub fn handle(&mut self) -> NPeekableHandle<I> {
        NPeekableHandle { offset: 0, reference: self }
    }
}

pub struct NPeekableHandle<'a, I: Iterator + 'a> {
    offset: usize,
    reference: &'a mut NPeekable<I>
}

impl<'a, I: Iterator> NPeekableHandle<'a, I> {
    pub fn peek(&mut self) -> Option<&I::Item> {
        self.offset += 1;
        let c = self.offset;
        self.reference.peek(c)
    }
    pub fn buffer(&mut self) -> Vec<I::Item> {
        let mut swapped = Vec::new();
        mem::swap(&mut swapped, &mut self.reference.queue);
        swapped
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn thing() {
        let c = vec![1,2,3,4];
        let mut iter = NPeekable::new(c.into_iter());
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.peek(0), Some(&2));
        assert_eq!(iter.peek(1), Some(&3));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(4));
    }
}
