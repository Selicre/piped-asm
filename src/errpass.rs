use std::marker::PhantomData;

#[derive(Clone)]
struct ErrPassInner<I,E> {
    inner: Rc<RefCell<(I,Option<E>)>>
}

impl<T, E, I: Iterator<Item=Result<T,E>>> Iterator for ErrPassInner<I,E> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        let inner = self.inner.borrow_mut();
        match inner.0.next() {
            Some(Ok(c)) => Some(c),
            Some(Err(e)) => { inner.1 = Some(e); None },
            None => None
        }
    }
}

struct ErrPass<T,U,I,E> {
    _marker: PhantomData<U>,
    iter: I,
    inner_ref: ErrPassInner<I,E>
}

// Src: R<T,E> -> ErrPassInner: T -> Adapter: U -> ErrPass: R<U,E>

impl<T,U,I,E> ErrPass<T,U,I,E> {
    fn new(old: ) -> 
}
