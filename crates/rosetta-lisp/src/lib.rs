//! # Rosetta LISP Frontend
//!
//! Parses Common Lisp and Scheme into Rosetta IR.

use rosetta_core::{Frontend, SourceFile, RosettaIr, ParseError};

pub struct LispParser {
    dialect: LispDialect,
}

#[derive(Clone, Copy)]
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
    fn name(&self) -> &'static str {
        match self.dialect {
            LispDialect::CommonLisp => "Common Lisp",
            LispDialect::Scheme => "Scheme",
        }
    }

    fn file_extensions(&self) -> &[&'static str] {
        match self.dialect {
            LispDialect::CommonLisp => &["lisp", "cl", "lsp"],
            LispDialect::Scheme => &["scm", "ss"],
        }
    }

    fn parse(&self, _source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        // TODO: Implement Lisp parser
        Ok(RosettaIr::default())
    }
}
