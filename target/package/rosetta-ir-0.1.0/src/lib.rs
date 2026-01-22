//! # Rosetta IR
//!
//! The Rosetta Intermediate Representation is a unified AST that represents
//! programs from any source language in a normalized form suitable for
//! Rust code generation.
//!
//! ## Design Goals
//!
//! 1. **Language Agnostic**: Can represent constructs from FORTRAN, COBOL, LISP,
//!    PLANNER, OPS5, KRL, Prolog, etc.
//! 2. **Type Complete**: Full type information for Rust codegen
//! 3. **Memory Model**: Tracks ownership, borrowing, mutability
//! 4. **Optimizable**: Amenable to transformations and analysis

pub use rosetta_core::{
    RosettaIr, IrNode, IrType, IrExpr, IrStmt, IrLiteral, BinOp, UnaryOp,
    SourceSpan, SourceLocation, SourceLanguage,
};

use rosetta_core::SourceLanguage as Lang;
use serde::{Deserialize, Serialize};

/// Extended IR module with full program structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IrModule {
    /// Module name (derived from source file)
    pub name: String,
    /// Source language
    pub source_lang: Lang,
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
    /// Global statements (for scripting languages)
    pub statements: Vec<IrExpr>,
    /// Entry point (if any)
    pub entry_point: Option<String>,
    /// Metadata
    pub metadata: IrMetadata,
}

impl Default for IrModule {
    fn default() -> Self {
        Self {
            name: String::new(),
            source_lang: Lang::Unknown,
            imports: Vec::new(),
            types: Vec::new(),
            constants: Vec::new(),
            globals: Vec::new(),
            functions: Vec::new(),
            statements: Vec::new(),
            entry_point: None,
            metadata: IrMetadata::default(),
        }
    }
}

impl IrModule {
    /// Convert to basic RosettaIr (for Frontend trait compatibility)
    pub fn into_rosetta_ir(self) -> rosetta_core::RosettaIr {
        use rosetta_core::IrNode;

        let mut nodes = Vec::new();

        // Convert type definitions
        for typedef in self.types {
            match typedef {
                IrTypeDef::Struct { name, fields, .. } => {
                    nodes.push(IrNode::Struct { name, fields });
                }
                IrTypeDef::Enum { name, .. } => {
                    nodes.push(IrNode::Comment(format!("enum {}", name)));
                }
                IrTypeDef::Alias { name, target } => {
                    nodes.push(IrNode::Comment(format!("type {} = {:?}", name, target)));
                }
            }
        }

        // Convert functions
        for func in self.functions {
            nodes.push(IrNode::Function {
                name: func.name,
                params: func.params.iter().map(|p| (p.name.clone(), p.ty.clone())).collect(),
                return_type: Some(func.return_type),
                body: func.body.iter().map(|e| IrNode::Expr(e.clone())).collect(),
            });
        }

        // Convert global statements
        for stmt in self.statements {
            nodes.push(IrNode::Expr(stmt));
        }

        rosetta_core::RosettaIr {
            name: self.name,
            nodes,
        }
    }
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
    /// Function body (as expressions)
    pub body: Vec<IrExpr>,
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
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Default)]
pub enum Visibility {
    #[default]
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
    current_function: Option<IrFunction>,
}

impl IrBuilder {
    /// Create a new IR builder with just a name (uses Unknown language)
    pub fn new(name: &str) -> Self {
        Self::with_language(name, Lang::Unknown)
    }

    /// Create a new IR builder with language
    pub fn with_language(name: &str, lang: Lang) -> Self {
        Self {
            module: IrModule {
                name: name.to_string(),
                source_lang: lang,
                ..Default::default()
            },
            current_function: None,
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

    /// Add a struct type definition
    pub fn add_struct(&mut self, name: &str, fields: &[(String, IrType)]) -> &mut Self {
        self.module.types.push(IrTypeDef::Struct {
            name: name.to_string(),
            fields: fields.to_vec(),
            derives: vec!["Debug".into(), "Clone".into()],
        });
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

    /// Begin building a new function
    pub fn begin_function(&mut self, name: &str, params: &[(String, IrType)], return_type: IrType) {
        self.current_function = Some(IrFunction {
            name: name.to_string(),
            generics: vec![],
            params: params.iter().map(|(n, t)| IrParam {
                name: n.clone(),
                ty: t.clone(),
                is_mutable: false,
                by_ref: false,
            }).collect(),
            return_type,
            body: vec![],
            is_unsafe: false,
            visibility: Visibility::Public,
            attributes: vec![],
            span: None,
        });
    }

    /// Add a statement to the current function
    pub fn add_statement(&mut self, expr: IrExpr) {
        if let Some(ref mut func) = self.current_function {
            func.body.push(expr);
        }
    }

    /// Add a return statement to the current function
    pub fn add_return(&mut self, expr: IrExpr) {
        if let Some(ref mut func) = self.current_function {
            func.body.push(IrExpr::Return(Box::new(expr)));
        }
    }

    /// End the current function and add it to the module
    pub fn end_function(&mut self) {
        if let Some(func) = self.current_function.take() {
            self.module.functions.push(func);
        }
    }

    /// Add a global statement (for scripting languages)
    pub fn add_global_statement(&mut self, expr: IrExpr) {
        self.module.statements.push(expr);
    }

    /// Set entry point
    pub fn set_entry_point(&mut self, name: &str) -> &mut Self {
        self.module.entry_point = Some(name.to_string());
        self
    }

    /// Build the final module (takes ownership)
    pub fn build(mut self) -> IrModule {
        // Make sure any pending function is added
        if let Some(func) = self.current_function.take() {
            self.module.functions.push(func);
        }
        self.module
    }

    /// Build a clone of the module (for use when you can't take ownership)
    pub fn build_clone(&mut self) -> IrModule {
        // Make sure any pending function is added
        if let Some(func) = self.current_function.take() {
            self.module.functions.push(func);
        }
        self.module.clone()
    }
}

/// Trait for converting language-specific ASTs to IR
pub trait ToIr<T> {
    fn to_ir(&self, ast: &T) -> Result<IrModule, rosetta_core::ParseError>;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ir_builder() {
        let mut builder = IrBuilder::with_language("test_module", Lang::Fortran77);

        builder.begin_function("main", &[], IrType::Int(32));
        builder.add_statement(IrExpr::Comment("Test function".into()));
        builder.add_return(IrExpr::Int(0));
        builder.end_function();

        builder.set_entry_point("main");

        let module = builder.build();
        assert_eq!(module.name, "test_module");
        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.entry_point, Some("main".to_string()));
    }

    #[test]
    fn test_add_struct() {
        let mut builder = IrBuilder::new("test");
        builder.add_struct("Point", &[
            ("x".into(), IrType::Float(64)),
            ("y".into(), IrType::Float(64)),
        ]);

        let module = builder.build();
        assert_eq!(module.types.len(), 1);
    }

    #[test]
    fn test_symbolic_language() {
        let mut builder = IrBuilder::with_language("prolog_module", Lang::Prolog);

        builder.begin_function("parent_2", &[
            ("X".into(), IrType::Any),
            ("Y".into(), IrType::Any),
        ], IrType::Bool);

        builder.add_statement(IrExpr::Unify {
            left: Box::new(IrExpr::Identifier("X".into())),
            right: Box::new(IrExpr::Symbol("tom".into())),
        });

        builder.add_return(IrExpr::Bool(true));
        builder.end_function();

        let module = builder.build();
        assert_eq!(module.source_lang, Lang::Prolog);
        assert!(module.source_lang.is_symbolic_ai());
    }
}
