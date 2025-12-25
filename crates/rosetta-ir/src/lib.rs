//! # Rosetta IR
//!
//! The Rosetta Intermediate Representation is a unified AST that represents
//! programs from any source language in a normalized form suitable for
//! Rust code generation.
//!
//! ## Design Goals
//!
//! 1. **Language Agnostic**: Can represent constructs from FORTRAN, COBOL, LISP, etc.
//! 2. **Type Complete**: Full type information for Rust codegen
//! 3. **Memory Model**: Tracks ownership, borrowing, mutability
//! 4. **Optimizable**: Amenable to transformations and analysis

pub use rosetta_core::{
    RosettaIr, IrNode, IrType, IrExpr, IrStmt, IrLiteral, BinOp, UnaryOp,
    SourceSpan, SourceLocation,
};

use serde::{Deserialize, Serialize};

/// Extended IR module with full program structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrModule {
    /// Module name (derived from source file)
    pub name: String,
    /// Source language
    pub source_lang: rosetta_core::SourceLanguage,
    /// Imports/uses
    pub imports: Vec<IrImport>,
    /// Type definitions (structs, enums)
    pub types: Vec<IrTypeDef>,
    /// Global constants
    pub constants: Vec<IrConstant>,
    /// Global variables
    pub globals: Vec<IrGlobal>,
    /// Functions
    pub functions: Vec<IrFunction>,
    /// Entry point (if any)
    pub entry_point: Option<String>,
    /// Metadata
    pub metadata: IrMetadata,
}

/// Import statement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrImport {
    /// Module path
    pub path: Vec<String>,
    /// Specific items (empty = wildcard)
    pub items: Vec<String>,
    /// Alias
    pub alias: Option<String>,
}

/// Type definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IrTypeDef {
    /// Struct
    Struct {
        name: String,
        fields: Vec<(String, IrType)>,
        derives: Vec<String>,
    },
    /// Enum
    Enum {
        name: String,
        variants: Vec<IrEnumVariant>,
    },
    /// Type alias
    Alias {
        name: String,
        target: IrType,
    },
}

/// Enum variant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrEnumVariant {
    pub name: String,
    pub fields: Option<Vec<IrType>>,
    pub discriminant: Option<i64>,
}

/// Global constant
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrConstant {
    pub name: String,
    pub ty: IrType,
    pub value: IrExpr,
    pub visibility: Visibility,
}

/// Global variable
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrGlobal {
    pub name: String,
    pub ty: IrType,
    pub init: Option<IrExpr>,
    pub is_mutable: bool,
    pub visibility: Visibility,
}

/// Function definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrFunction {
    /// Function name
    pub name: String,
    /// Generic parameters
    pub generics: Vec<IrGeneric>,
    /// Parameters
    pub params: Vec<IrParam>,
    /// Return type
    pub return_type: IrType,
    /// Function body
    pub body: Vec<IrNode>,
    /// Is this function unsafe
    pub is_unsafe: bool,
    /// Visibility
    pub visibility: Visibility,
    /// Attributes (inline, must_use, etc.)
    pub attributes: Vec<String>,
    /// Original source span
    pub span: Option<SourceSpan>,
}

/// Generic parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrGeneric {
    pub name: String,
    pub bounds: Vec<String>,
}

/// Function parameter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrParam {
    pub name: String,
    pub ty: IrType,
    pub is_mutable: bool,
    /// Pass by reference (for FORTRAN)
    pub by_ref: bool,
}

/// Visibility
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Visibility {
    Public,
    Private,
    Crate,
    Super,
}

/// Module metadata
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct IrMetadata {
    /// Original source file
    pub source_file: Option<String>,
    /// Original language version
    pub language_version: Option<String>,
    /// Transpilation options used
    pub options: Vec<(String, String)>,
    /// Warnings generated during analysis
    pub warnings: Vec<String>,
}

/// IR builder for constructing modules
pub struct IrBuilder {
    module: IrModule,
}

impl IrBuilder {
    /// Create a new IR builder
    pub fn new(name: &str, lang: rosetta_core::SourceLanguage) -> Self {
        Self {
            module: IrModule {
                name: name.to_string(),
                source_lang: lang,
                imports: Vec::new(),
                types: Vec::new(),
                constants: Vec::new(),
                globals: Vec::new(),
                functions: Vec::new(),
                entry_point: None,
                metadata: IrMetadata::default(),
            },
        }
    }

    /// Add an import
    pub fn add_import(&mut self, import: IrImport) -> &mut Self {
        self.module.imports.push(import);
        self
    }

    /// Add a type definition
    pub fn add_type(&mut self, typedef: IrTypeDef) -> &mut Self {
        self.module.types.push(typedef);
        self
    }

    /// Add a constant
    pub fn add_constant(&mut self, constant: IrConstant) -> &mut Self {
        self.module.constants.push(constant);
        self
    }

    /// Add a global variable
    pub fn add_global(&mut self, global: IrGlobal) -> &mut Self {
        self.module.globals.push(global);
        self
    }

    /// Add a function
    pub fn add_function(&mut self, function: IrFunction) -> &mut Self {
        self.module.functions.push(function);
        self
    }

    /// Set entry point
    pub fn set_entry_point(&mut self, name: &str) -> &mut Self {
        self.module.entry_point = Some(name.to_string());
        self
    }

    /// Build the final module
    pub fn build(self) -> IrModule {
        self.module
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rosetta_core::SourceLanguage;

    #[test]
    fn test_ir_builder() {
        let mut builder = IrBuilder::new("test_module", SourceLanguage::Fortran77);

        builder
            .add_function(IrFunction {
                name: "main".to_string(),
                generics: vec![],
                params: vec![],
                return_type: IrType::Int(32),
                body: vec![],
                is_unsafe: false,
                visibility: Visibility::Public,
                attributes: vec![],
                span: None,
            })
            .set_entry_point("main");

        let module = builder.build();
        assert_eq!(module.name, "test_module");
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.entry_point, Some("main".to_string()));
    }
}
