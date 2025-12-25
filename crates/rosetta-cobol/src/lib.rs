//! # Rosetta COBOL Frontend
//!
//! Parses COBOL source code into Rosetta IR.
//! Supports COBOL-85 and later versions.

use rosetta_core::{Frontend, SourceLanguage, Result};

pub struct CobolParser;

impl Frontend for CobolParser {
    type Ast = ();

    fn parse(&self, _source: &str) -> Result<Self::Ast> {
        todo!("COBOL parser not yet implemented")
    }

    fn language(&self) -> SourceLanguage {
        SourceLanguage::Cobol
    }
}
