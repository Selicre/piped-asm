use lexer::Span;

#[derive(Debug)]
pub enum Attribute {
    Bank(u8),
    Pin(u32),
    WarnLength(u16)
}

#[derive(Debug)]
pub enum AttributeError {
    WrongArgType,
    UnexpectedEnd,
    NotFound,
    Other
}



impl Attribute {
    // TODO: base this on an iterator
    pub fn from_span(s: &[Span]) -> Result<Self,AttributeError> {
        use self::Attribute::*;
        use self::AttributeError::*;
        let ident = s.get(0)
            .ok_or(UnexpectedEnd)?
            .as_ident()
            .ok_or(Other)?;
        Ok(match ident {
            "bank" => {
                Bank(s.get(2).ok_or(UnexpectedEnd)?.as_number().ok_or(WrongArgType)? as u8)
            },
            _ => return Err(NotFound)
        })
    }
}
