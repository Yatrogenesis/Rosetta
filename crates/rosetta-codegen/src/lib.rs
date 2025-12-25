//! # Rosetta Code Generator
//!
//! Generates idiomatic Rust code from Rosetta IR.
//!
//! ## Strategies
//!
//! 1. **Safe First**: Generate safe Rust when possible
//! 2. **Unsafe Fallback**: Use unsafe for exact numerical equivalence
//! 3. **Idiomatic**: Use Rust idioms (iterators, pattern matching)
//! 4. **Documented**: Include comments about original code

use rosetta_core::{IrType, IrExpr, IrStmt, IrLiteral, BinOp, UnaryOp, TranspileError, Result};
use rosetta_ir::{IrModule, IrFunction, IrTypeDef, Visibility};
use proc_macro2::TokenStream;
use quote::{quote, format_ident};

/// Code generation options
#[derive(Debug, Clone)]
pub struct CodegenOptions {
    /// Generate unsafe code for exact equivalence
    pub allow_unsafe: bool,
    /// Include comments with original source
    pub include_comments: bool,
    /// Format output with rustfmt
    pub format_output: bool,
    /// Generate tests alongside code
    pub generate_tests: bool,
    /// Use ndarray for arrays
    pub use_ndarray: bool,
}

impl Default for CodegenOptions {
    fn default() -> Self {
        Self {
            allow_unsafe: true,
            include_comments: true,
            format_output: true,
            generate_tests: false,
            use_ndarray: true,
        }
    }
}

/// Rust code generator
pub struct RustCodegen {
    options: CodegenOptions,
}

impl RustCodegen {
    /// Create a new code generator
    pub fn new(options: CodegenOptions) -> Self {
        Self { options }
    }

    /// Generate Rust code from IR module
    pub fn generate(&self, module: &IrModule) -> Result<String> {
        let tokens = self.generate_tokens(module)?;

        if self.options.format_output {
            self.format_code(tokens)
        } else {
            Ok(tokens.to_string())
        }
    }

    /// Generate token stream from IR module
    fn generate_tokens(&self, module: &IrModule) -> Result<TokenStream> {
        let module_comment = if self.options.include_comments {
            let lang = format!("{:?}", module.source_lang);
            quote! {
                //! Transpiled from #lang by Rosetta
                //! Original file: #module.metadata.source_file
            }
        } else {
            quote! {}
        };

        // Generate imports
        let imports = self.generate_imports(module)?;

        // Generate type definitions
        let types: Vec<TokenStream> = module.types
            .iter()
            .map(|t| self.generate_type_def(t))
            .collect::<Result<_>>()?;

        // Generate constants
        let constants: Vec<TokenStream> = module.constants
            .iter()
            .map(|c| {
                let name = format_ident!("{}", &c.name);
                let ty = self.ir_type_to_rust(&c.ty)?;
                let value = self.expr_to_tokens(&c.value)?;
                let vis = self.visibility_to_tokens(c.visibility);
                Ok(quote! {
                    #vis const #name: #ty = #value;
                })
            })
            .collect::<Result<_>>()?;

        // Generate functions
        let functions: Vec<TokenStream> = module.functions
            .iter()
            .map(|f| self.generate_function(f))
            .collect::<Result<_>>()?;

        Ok(quote! {
            #module_comment
            #imports
            #(#types)*
            #(#constants)*
            #(#functions)*
        })
    }

    /// Generate imports
    fn generate_imports(&self, _module: &IrModule) -> Result<TokenStream> {
        let mut imports = vec![];

        if self.options.use_ndarray {
            imports.push(quote! { use ndarray::{Array1, Array2, ArrayD}; });
        }

        Ok(quote! { #(#imports)* })
    }

    /// Generate type definition
    fn generate_type_def(&self, typedef: &IrTypeDef) -> Result<TokenStream> {
        match typedef {
            IrTypeDef::Struct { name, fields, derives } => {
                let name_ident = format_ident!("{}", name);
                let derive_idents: Vec<_> = derives.iter()
                    .map(|d| format_ident!("{}", d))
                    .collect();

                let field_tokens: Vec<TokenStream> = fields.iter()
                    .map(|(name, ty)| {
                        let field_name = format_ident!("{}", name);
                        let field_ty = self.ir_type_to_rust(ty)?;
                        Ok(quote! { pub #field_name: #field_ty })
                    })
                    .collect::<Result<_>>()?;

                Ok(quote! {
                    #[derive(#(#derive_idents),*)]
                    pub struct #name_ident {
                        #(#field_tokens),*
                    }
                })
            }
            IrTypeDef::Enum { name, variants } => {
                let name_ident = format_ident!("{}", name);
                let variant_tokens: Vec<TokenStream> = variants.iter()
                    .map(|v| {
                        let var_name = format_ident!("{}", &v.name);
                        if let Some(disc) = v.discriminant {
                            quote! { #var_name = #disc }
                        } else {
                            quote! { #var_name }
                        }
                    })
                    .collect();

                Ok(quote! {
                    pub enum #name_ident {
                        #(#variant_tokens),*
                    }
                })
            }
            IrTypeDef::Alias { name, target } => {
                let name_ident = format_ident!("{}", name);
                let target_ty = self.ir_type_to_rust(target)?;
                Ok(quote! {
                    pub type #name_ident = #target_ty;
                })
            }
        }
    }

    /// Generate function
    fn generate_function(&self, func: &IrFunction) -> Result<TokenStream> {
        let name = format_ident!("{}", &func.name);
        let vis = self.visibility_to_tokens(func.visibility);

        let params: Vec<TokenStream> = func.params.iter()
            .map(|p| {
                let param_name = format_ident!("{}", &p.name);
                let param_ty = self.ir_type_to_rust(&p.ty)?;
                if p.by_ref && p.is_mutable {
                    Ok(quote! { #param_name: &mut #param_ty })
                } else if p.by_ref {
                    Ok(quote! { #param_name: &#param_ty })
                } else {
                    Ok(quote! { #param_name: #param_ty })
                }
            })
            .collect::<Result<_>>()?;

        let return_ty = self.ir_type_to_rust(&func.return_type)?;

        let body_tokens: Vec<TokenStream> = func.body.iter()
            .map(|node| self.node_to_tokens(node))
            .collect::<Result<_>>()?;

        let unsafe_kw = if func.is_unsafe {
            quote! { unsafe }
        } else {
            quote! {}
        };

        Ok(quote! {
            #vis #unsafe_kw fn #name(#(#params),*) -> #return_ty {
                #(#body_tokens)*
            }
        })
    }

    /// Convert IR type to Rust type
    fn ir_type_to_rust(&self, ty: &IrType) -> Result<TokenStream> {
        match ty {
            IrType::Int(8) => Ok(quote! { i8 }),
            IrType::Int(16) => Ok(quote! { i16 }),
            IrType::Int(32) => Ok(quote! { i32 }),
            IrType::Int(64) => Ok(quote! { i64 }),
            IrType::Int(_) => Ok(quote! { i32 }),
            IrType::Float(32) => Ok(quote! { f32 }),
            IrType::Float(64) => Ok(quote! { f64 }),
            IrType::Float(_) => Ok(quote! { f64 }),
            IrType::Bool => Ok(quote! { bool }),
            IrType::String => Ok(quote! { String }),
            IrType::Array(inner, Some(size)) => {
                let inner_ty = self.ir_type_to_rust(inner)?;
                let size_lit = proc_macro2::Literal::usize_unsuffixed(*size);
                Ok(quote! { [#inner_ty; #size_lit] })
            }
            IrType::Array(inner, None) => {
                let inner_ty = self.ir_type_to_rust(inner)?;
                Ok(quote! { Vec<#inner_ty> })
            }
            IrType::Ref(inner) => {
                let inner_ty = self.ir_type_to_rust(inner)?;
                Ok(quote! { &#inner_ty })
            }
            IrType::MutRef(inner) => {
                let inner_ty = self.ir_type_to_rust(inner)?;
                Ok(quote! { &mut #inner_ty })
            }
            IrType::Struct(name) => {
                let name_ident = format_ident!("{}", name);
                Ok(quote! { #name_ident })
            }
            IrType::Unknown => Ok(quote! { () }),
        }
    }

    /// Convert IR node to tokens
    fn node_to_tokens(&self, _node: &rosetta_core::IrNode) -> Result<TokenStream> {
        // Placeholder - full implementation would handle all node types
        Ok(quote! {})
    }

    /// Convert expression to tokens
    fn expr_to_tokens(&self, expr: &IrExpr) -> Result<TokenStream> {
        match expr {
            IrExpr::Literal(lit) => self.literal_to_tokens(lit),
            IrExpr::Var(name) => {
                let ident = format_ident!("{}", name);
                Ok(quote! { #ident })
            }
            IrExpr::BinOp { op, left, right } => {
                let left_tokens = self.expr_to_tokens(left)?;
                let right_tokens = self.expr_to_tokens(right)?;
                let op_tokens = self.binop_to_tokens(*op);
                Ok(quote! { (#left_tokens #op_tokens #right_tokens) })
            }
            IrExpr::UnaryOp { op, operand } => {
                let operand_tokens = self.expr_to_tokens(operand)?;
                let op_tokens = self.unaryop_to_tokens(*op);
                Ok(quote! { #op_tokens #operand_tokens })
            }
            IrExpr::Call { func, args } => {
                let func_ident = format_ident!("{}", func);
                let arg_tokens: Vec<TokenStream> = args.iter()
                    .map(|a| self.expr_to_tokens(a))
                    .collect::<Result<_>>()?;
                Ok(quote! { #func_ident(#(#arg_tokens),*) })
            }
            IrExpr::Index { array, index } => {
                let arr = self.expr_to_tokens(array)?;
                let idx = self.expr_to_tokens(index)?;
                Ok(quote! { #arr[#idx] })
            }
            IrExpr::Field { object, field } => {
                let obj = self.expr_to_tokens(object)?;
                let fld = format_ident!("{}", field);
                Ok(quote! { #obj.#fld })
            }
        }
    }

    /// Convert literal to tokens
    fn literal_to_tokens(&self, lit: &IrLiteral) -> Result<TokenStream> {
        match lit {
            IrLiteral::Int(i) => Ok(quote! { #i }),
            IrLiteral::Float(f) => {
                let f_lit = proc_macro2::Literal::f64_unsuffixed(*f);
                Ok(quote! { #f_lit })
            }
            IrLiteral::Bool(b) => Ok(quote! { #b }),
            IrLiteral::String(s) => Ok(quote! { #s.to_string() }),
            IrLiteral::Char(c) => Ok(quote! { #c }),
        }
    }

    /// Convert binary operator to tokens
    fn binop_to_tokens(&self, op: BinOp) -> TokenStream {
        match op {
            BinOp::Add => quote! { + },
            BinOp::Sub => quote! { - },
            BinOp::Mul => quote! { * },
            BinOp::Div => quote! { / },
            BinOp::Mod => quote! { % },
            BinOp::Eq => quote! { == },
            BinOp::Ne => quote! { != },
            BinOp::Lt => quote! { < },
            BinOp::Le => quote! { <= },
            BinOp::Gt => quote! { > },
            BinOp::Ge => quote! { >= },
            BinOp::And => quote! { && },
            BinOp::Or => quote! { || },
            BinOp::Xor => quote! { ^ },
            BinOp::BitAnd => quote! { & },
            BinOp::BitOr => quote! { | },
            BinOp::BitXor => quote! { ^ },
            BinOp::Shl => quote! { << },
            BinOp::Shr => quote! { >> },
        }
    }

    /// Convert unary operator to tokens
    fn unaryop_to_tokens(&self, op: UnaryOp) -> TokenStream {
        match op {
            UnaryOp::Neg => quote! { - },
            UnaryOp::Not => quote! { ! },
            UnaryOp::BitNot => quote! { ! },
            UnaryOp::Deref => quote! { * },
            UnaryOp::Ref => quote! { & },
        }
    }

    /// Convert visibility to tokens
    fn visibility_to_tokens(&self, vis: Visibility) -> TokenStream {
        match vis {
            Visibility::Public => quote! { pub },
            Visibility::Private => quote! {},
            Visibility::Crate => quote! { pub(crate) },
            Visibility::Super => quote! { pub(super) },
        }
    }

    /// Format code using prettyplease
    fn format_code(&self, tokens: TokenStream) -> Result<String> {
        let file: syn::File = syn::parse2(tokens)
            .map_err(|e| TranspileError::CodegenError(format!("Parse error: {}", e)))?;

        Ok(prettyplease::unparse(&file))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rosetta_core::SourceLanguage;
    use rosetta_ir::IrBuilder;

    #[test]
    fn test_basic_codegen() {
        let mut builder = IrBuilder::new("test", SourceLanguage::Fortran77);
        builder.add_function(rosetta_ir::IrFunction {
            name: "add".to_string(),
            generics: vec![],
            params: vec![
                rosetta_ir::IrParam {
                    name: "a".to_string(),
                    ty: IrType::Int(32),
                    is_mutable: false,
                    by_ref: false,
                },
                rosetta_ir::IrParam {
                    name: "b".to_string(),
                    ty: IrType::Int(32),
                    is_mutable: false,
                    by_ref: false,
                },
            ],
            return_type: IrType::Int(32),
            body: vec![],
            is_unsafe: false,
            visibility: Visibility::Public,
            attributes: vec![],
            span: None,
        });

        let module = builder.build();
        let codegen = RustCodegen::new(CodegenOptions::default());
        let result = codegen.generate(&module);

        assert!(result.is_ok());
    }
}
