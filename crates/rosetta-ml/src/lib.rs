//! # Rosetta ML Frontend
//!
//! Parses Standard ML and OCaml into Rosetta IR.

use rosetta_core::{Frontend, SourceFile, RosettaIr, ParseError};

pub struct MLParser {
    dialect: MLDialect,
}

#[derive(Clone, Copy)]
pub enum MLDialect {
    StandardML,
    OCaml,
}

impl MLParser {
    pub fn standard_ml() -> Self {
        Self { dialect: MLDialect::StandardML }
    }

    pub fn ocaml() -> Self {
        Self { dialect: MLDialect::OCaml }
    }
}

impl Frontend for MLParser {
    fn name(&self) -> &'static str {
        match self.dialect {
            MLDialect::StandardML => "Standard ML",
            MLDialect::OCaml => "OCaml",
        }
    }

    fn file_extensions(&self) -> &[&'static str] {
        match self.dialect {
            MLDialect::StandardML => &["sml", "sig"],
            MLDialect::OCaml => &["ml", "mli"],
        }
    }

    fn parse(&self, _source: &SourceFile) -> std::result::Result<RosettaIr, ParseError> {
        // TODO: Implement ML parser
        Ok(RosettaIr::default())
    }
}
