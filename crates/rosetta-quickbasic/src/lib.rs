//! # Rosetta QuickBASIC Frontend
//!
//! Parses QuickBASIC/QBASIC into Rosetta IR.

use rosetta_core::{Frontend, SourceFile, RosettaIr, ParseError};

pub struct QuickBasicParser;

impl Frontend for QuickBasicParser {
    fn name(&self) -> &'static str {
        "QuickBASIC"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["bas", "bi"]
    }

    fn parse(&self, _source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        // TODO: Implement QuickBASIC parser
        Ok(RosettaIr::default())
    }
}
