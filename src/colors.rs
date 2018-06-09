use std::fmt::{self,Display};
use std::ops::Add;

pub struct PrettyPrinted<D> {
    content: D,
    color: Option<u8>,
    bold: bool
}

impl<D: Display> fmt::Display for PrettyPrinted<D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("\x1B[0")?;
        if let Some(c) = self.color {
            write!(f, ";38;5;{}",c)?;
        }
        if self.bold {
            f.write_str(";1")?;
        }
        f.write_str("m")?;
        write!(f,"{}", self.content)?;
        f.write_str("\x1B[0m")?;
        Ok(())
    }
}

pub enum PrettyPrintOption {
    Color(u8),
    Bold
}

impl<D> Add<PrettyPrintOption> for PrettyPrinted<D> {
    type Output = PrettyPrinted<D>;
    fn add(mut self, other: PrettyPrintOption) -> Self::Output {
        use self::PrettyPrintOption::*;
        match other {
            Color(c) => self.color = Some(c),
            Bold => self.bold = true
        }
        self
    }
}

pub trait PrettyPrintExt {
    fn pretty(&self) -> PrettyPrinted<&Self>;
}

impl<D: Display> PrettyPrintExt for D {
    fn pretty(&self) -> PrettyPrinted<&Self> {
        PrettyPrinted { content: self, color: None, bold: false }
    }
}

pub mod prelude {
    pub use super::PrettyPrintOption::*;
    pub use super::PrettyPrintExt;
}
