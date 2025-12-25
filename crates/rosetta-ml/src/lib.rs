//! # Rosetta ML Frontend
//!
//! Parses Standard ML and OCaml into Rosetta IR.
//! Functional programming paradigm support.

use rosetta_core::{Frontend, SourceLanguage, Result};

pub struct MLParser {
    dialect: MLDialect,
}

pub enum MLDialect {
    StandardML,
    OCaml,
}

impl MLParser {
    pub fn sml() -> Self {
        Self { dialect: MLDialect::StandardML }
    }

    pub fn ocaml() -> Self {
        Self { dialect: MLDialect::OCaml }
    }
}

impl Frontend for MLParser {
    type Ast = ();

    fn parse(&self, _source: &str) -> Result<Self::Ast> {
        todo!("ML parser not yet implemented")
    }

    fn language(&self) -> SourceLanguage {
        match self.dialect {
            MLDialect::StandardML => SourceLanguage::StandardML,
            MLDialect::OCaml => SourceLanguage::OCaml,
        }
    }
}
