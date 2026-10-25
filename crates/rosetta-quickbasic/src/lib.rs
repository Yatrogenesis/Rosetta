//! # Rosetta QuickBASIC Frontend
//!
//! Parses QuickBASIC/QBASIC into Rosetta IR.

use rosetta_stone_core::{Frontend, SourceFile, RosettaIr, ParseError};

pub struct QuickBasicParser;

impl Frontend for QuickBasicParser {
    fn name(&self) -> &'static str {
        "QuickBASIC"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["bas", "bi"]
    }

    fn parse(&self, _source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        // Not implemented: fail loudly instead of returning an empty IR.
        Err(ParseError::Unsupported("QuickBASIC frontend is not implemented yet".to_string()))
    }
}
