//! # Rosetta LISP Frontend
//!
//! Parses Common Lisp and Scheme into Rosetta IR.
//! Critical for PIRS-LIRS integration with genesis01.

use rosetta_core::{Frontend, SourceLanguage, Result};

pub struct LispParser {
    dialect: LispDialect,
}

pub enum LispDialect {
    CommonLisp,
    Scheme,
}

impl LispParser {
    pub fn common_lisp() -> Self {
        Self { dialect: LispDialect::CommonLisp }
    }

    pub fn scheme() -> Self {
        Self { dialect: LispDialect::Scheme }
    }
}

impl Frontend for LispParser {
    type Ast = ();

    fn parse(&self, _source: &str) -> Result<Self::Ast> {
        todo!("LISP parser not yet implemented")
    }

    fn language(&self) -> SourceLanguage {
        match self.dialect {
            LispDialect::CommonLisp => SourceLanguage::CommonLisp,
            LispDialect::Scheme => SourceLanguage::Scheme,
        }
    }
}
