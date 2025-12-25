//! # Rosetta COBOL Frontend
//!
//! Parses COBOL source code into Rosetta IR.
//! Supports COBOL-85 and later versions.

use rosetta_core::{Frontend, SourceFile, RosettaIr, ParseError};

pub struct CobolParser;

impl Frontend for CobolParser {
    fn name(&self) -> &'static str {
        "COBOL"
    }

    fn file_extensions(&self) -> &[&'static str] {
        &["cob", "cbl", "cpy"]
    }

    fn parse(&self, _source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        // TODO: Implement COBOL parser
        Ok(RosettaIr::default())
    }
}
