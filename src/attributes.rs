use lexer::Span;

#[derive(Clone, Debug)]
pub enum Attribute {
    Bank(u8),
    Pin(u32),
    WarnLength(u16),
    SpanBanks(bool),
    Start,
    NMI,
    IRQ,
    BRK
}

#[derive(Debug)]
pub enum AttributeError {
    WrongArgType,
    UnexpectedEnd,
    NotFound,
    WrongAttrName(Span),
    Other
}



impl Attribute {
    pub fn from_span(s: &[Span]) -> Result<Self,AttributeError> {
        use self::Attribute::*;
        use self::AttributeError::*;
        let ident = s.get(0)
            .ok_or(UnexpectedEnd)?;
        let ident = ident
            .as_ident()
            .ok_or(WrongAttrName(ident.clone()))?;
        Ok(match ident {
            "bank" => {
                Bank(s.get(2).ok_or(UnexpectedEnd)?.as_number().ok_or(WrongArgType)? as u8)
            },
            "span_banks" => {
                SpanBanks(s.get(2).map(|c| {
                    c.as_ident().ok_or(AttributeError::WrongArgType)?
                        .parse().map_err(|_| AttributeError::WrongArgType)
                }).unwrap_or(Ok(true))?)
            },
            "start" => Start,
            "nmi" => NMI,
            "irq" => IRQ,
            "brk" => BRK,
            _ => return Err(NotFound)
        })
    }
}
