//! # Rosetta QuickBASIC Frontend
//!
//! Parses QuickBASIC/QBASIC into Rosetta IR.
//! Educational accessibility for scientists.

use rosetta_core::{Frontend, SourceLanguage, Result};

pub struct QuickBasicParser;

impl Frontend for QuickBasicParser {
    type Ast = ();

    fn parse(&self, _source: &str) -> Result<Self::Ast> {
        todo!("QuickBASIC parser not yet implemented")
    }

    fn language(&self) -> SourceLanguage {
        SourceLanguage::QuickBasic
    }
}
