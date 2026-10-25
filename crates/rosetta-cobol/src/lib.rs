//! # Rosetta COBOL Frontend
//!
//! Parses COBOL source code into Rosetta IR.
//! Supports COBOL-85 and later versions.

use rosetta_stone_core::{Frontend, SourceFile, RosettaIr, ParseError};

pub struct CobolParser;

impl Frontend for CobolParser {
    fn name(&self) -> &'static str {
        "COBOL"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["cob", "cbl", "cpy"]
    }

    fn parse(&self, _source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        // Not implemented: fail loudly instead of returning an empty IR.
        Err(ParseError::Unsupported("COBOL frontend is not implemented yet".to_string()))
    }
}
