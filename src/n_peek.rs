// Iterator tool, like Peekable, but any amount of peeks

use std::collections::VecDeque;
use std::mem;

pub struct NPeekable<I: Iterator> {
    queue: VecDeque<I::Item>,
    iter: I
}

impl<I: Iterator> Iterator for NPeekable<I> {
    type Item = I::Item;
    fn next(&mut self) -> Option<I::Item> {
        if self.queue.len() > 0 {
            self.queue.pop_front()
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
            self.queue.push_back(next);
        }
        Some(&self.queue[offset])
    }
    pub fn new(iter: I) -> Self {
        Self { iter, queue: VecDeque::new() }
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
    pub fn next(&mut self) -> Option<&I::Item> {
        self.offset += 1;
        self.reference.peek(self.offset - 1)
    }
    pub fn peek(&mut self, offset: usize) -> Option<&I::Item> {
        self.reference.peek(self.offset + offset)
    }
    pub fn buffer(&mut self) -> VecDeque<I::Item> {
        let mut swapped = VecDeque::new();
        mem::swap(&mut swapped, &mut self.reference.queue);
        swapped
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn consistency() {
        let c = vec![1,2,3,4];
        let mut iter = NPeekable::new(c.into_iter());
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.peek(0), Some(&2));
        assert_eq!(iter.peek(1), Some(&3));
        assert_eq!(iter.next(), Some(2));
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(4));
    }
    #[test]
    fn handle() {
        let c = vec![1,2,3,4];
        let mut iter = NPeekable::new(c.into_iter());
        {
            let mut h1 = iter.handle();
            assert_eq!(h1.next(), Some(&1));
            assert_eq!(h1.next(), Some(&2));
            assert_eq!(h1.next(), Some(&3));
        }
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(2));
        {
            let mut h1 = iter.handle();
            assert_eq!(h1.next(), Some(&3));
            assert_eq!(h1.next(), Some(&4));
            assert_eq!(h1.next(), None);
        }
        assert_eq!(iter.next(), Some(3));
        assert_eq!(iter.next(), Some(4));
        assert_eq!(iter.next(), None);
    }
}
