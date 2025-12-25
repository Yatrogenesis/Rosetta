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

use rosetta_core::{IrType, IrExpr, IrLiteral, TranspileError, Result};
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
            .map(|expr| self.expr_to_tokens(expr))
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
            IrType::Char => Ok(quote! { char }),
            IrType::Unit => Ok(quote! { () }),
            IrType::Array(inner, Some(size)) => {
                let inner_ty = self.ir_type_to_rust(inner)?;
                let size_lit = proc_macro2::Literal::usize_unsuffixed(*size);
                Ok(quote! { [#inner_ty; #size_lit] })
            }
            IrType::Array(inner, None) => {
                let inner_ty = self.ir_type_to_rust(inner)?;
                Ok(quote! { Vec<#inner_ty> })
            }
            IrType::Vec(inner) => {
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
            IrType::Box(inner) => {
                let inner_ty = self.ir_type_to_rust(inner)?;
                Ok(quote! { Box<#inner_ty> })
            }
            IrType::Option(inner) => {
                let inner_ty = self.ir_type_to_rust(inner)?;
                Ok(quote! { Option<#inner_ty> })
            }
            IrType::Result(ok, err) => {
                let ok_ty = self.ir_type_to_rust(ok)?;
                let err_ty = self.ir_type_to_rust(err)?;
                Ok(quote! { Result<#ok_ty, #err_ty> })
            }
            IrType::Tuple(types) => {
                let type_tokens: Vec<TokenStream> = types.iter()
                    .map(|t| self.ir_type_to_rust(t))
                    .collect::<Result<_>>()?;
                Ok(quote! { (#(#type_tokens),*) })
            }
            IrType::Struct(name) => {
                let name_ident = format_ident!("{}", name);
                Ok(quote! { #name_ident })
            }
            IrType::Fn(params, ret) => {
                let param_types: Vec<TokenStream> = params.iter()
                    .map(|t| self.ir_type_to_rust(t))
                    .collect::<Result<_>>()?;
                let ret_ty = self.ir_type_to_rust(ret)?;
                Ok(quote! { fn(#(#param_types),*) -> #ret_ty })
            }
            IrType::Iterator(inner) => {
                let inner_ty = self.ir_type_to_rust(inner)?;
                Ok(quote! { impl Iterator<Item = #inner_ty> })
            }
            IrType::Any => Ok(quote! { Box<dyn std::any::Any> }),
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
            // === Literals ===
            IrExpr::Int(i) => Ok(quote! { #i }),
            IrExpr::Float(f) => {
                let f_lit = proc_macro2::Literal::f64_unsuffixed(*f);
                Ok(quote! { #f_lit })
            }
            IrExpr::Bool(b) => Ok(quote! { #b }),
            IrExpr::String(s) => Ok(quote! { #s.to_string() }),
            IrExpr::Char(c) => Ok(quote! { #c }),
            IrExpr::Nil => Ok(quote! { None }),
            IrExpr::Symbol(s) => {
                let sym = format_ident!("{}", s);
                Ok(quote! { #sym })
            }

            // === Variables ===
            IrExpr::Identifier(name) => {
                let ident = format_ident!("{}", name);
                Ok(quote! { #ident })
            }
            IrExpr::PatternVar(name) => {
                let ident = format_ident!("{}", name);
                Ok(quote! { #ident })
            }

            // === Operators ===
            IrExpr::BinaryOp { op, left, right } => {
                let left_tokens = self.expr_to_tokens(left)?;
                let right_tokens = self.expr_to_tokens(right)?;
                let op_tokens = self.string_binop_to_tokens(op);
                Ok(quote! { (#left_tokens #op_tokens #right_tokens) })
            }
            IrExpr::UnaryOp { op, operand } => {
                let operand_tokens = self.expr_to_tokens(operand)?;
                let op_tokens = self.string_unaryop_to_tokens(op);
                Ok(quote! { #op_tokens #operand_tokens })
            }

            // === Function Calls ===
            IrExpr::Call { func, args } => {
                let func_tokens = self.expr_to_tokens(func)?;
                let arg_tokens: Vec<TokenStream> = args.iter()
                    .map(|a| self.expr_to_tokens(a))
                    .collect::<Result<_>>()?;
                Ok(quote! { #func_tokens(#(#arg_tokens),*) })
            }

            // === Data Structures ===
            IrExpr::List(items) => {
                let item_tokens: Vec<TokenStream> = items.iter()
                    .map(|i| self.expr_to_tokens(i))
                    .collect::<Result<_>>()?;
                Ok(quote! { vec![#(#item_tokens),*] })
            }
            IrExpr::ListCons { head, tail } => {
                let h = self.expr_to_tokens(head)?;
                let t = self.expr_to_tokens(tail)?;
                Ok(quote! { std::iter::once(#h).chain(#t.into_iter()).collect::<Vec<_>>() })
            }
            IrExpr::StructInit { name, fields } => {
                let name_ident = format_ident!("{}", name);
                let field_inits: Vec<TokenStream> = fields.iter()
                    .map(|(fname, fval)| {
                        let fi = format_ident!("{}", fname);
                        let fv = self.expr_to_tokens(fval)?;
                        Ok(quote! { #fi: #fv })
                    })
                    .collect::<Result<_>>()?;
                Ok(quote! { #name_ident { #(#field_inits),* } })
            }
            IrExpr::Tuple(items) => {
                let item_tokens: Vec<TokenStream> = items.iter()
                    .map(|i| self.expr_to_tokens(i))
                    .collect::<Result<_>>()?;
                Ok(quote! { (#(#item_tokens),*) })
            }

            // === Access ===
            IrExpr::FieldAccess { object, field } => {
                let obj = self.expr_to_tokens(object)?;
                let fld = format_ident!("{}", field);
                Ok(quote! { #obj.#fld })
            }
            IrExpr::FieldAssign { object, field, value } => {
                let obj = format_ident!("{}", object);
                let fld = format_ident!("{}", field);
                let val = self.expr_to_tokens(value)?;
                Ok(quote! { #obj.#fld = #val })
            }
            IrExpr::Index { array, index } => {
                let arr = self.expr_to_tokens(array)?;
                let idx = self.expr_to_tokens(index)?;
                Ok(quote! { #arr[#idx] })
            }

            // === Control Flow ===
            IrExpr::If { condition, then_branch, else_branch } => {
                let cond = self.expr_to_tokens(condition)?;
                let then_b = self.expr_to_tokens(then_branch)?;
                if let Some(else_b) = else_branch {
                    let else_tokens = self.expr_to_tokens(else_b)?;
                    Ok(quote! { if #cond { #then_b } else { #else_tokens } })
                } else {
                    Ok(quote! { if #cond { #then_b } })
                }
            }
            IrExpr::Cond(branches) => {
                if branches.is_empty() {
                    return Ok(quote! { () });
                }
                let mut tokens = TokenStream::new();
                for (i, (cond, body)) in branches.iter().enumerate() {
                    let c = self.expr_to_tokens(cond)?;
                    let b = self.expr_to_tokens(body)?;
                    if i == 0 {
                        tokens = quote! { if #c { #b } };
                    } else {
                        tokens = quote! { #tokens else if #c { #b } };
                    }
                }
                Ok(tokens)
            }
            IrExpr::Match { scrutinee, arms } => {
                let scrut = self.expr_to_tokens(scrutinee)?;
                let arm_tokens: Vec<TokenStream> = arms.iter()
                    .map(|(pat, body)| {
                        let p = self.expr_to_tokens(pat)?;
                        let b = self.expr_to_tokens(body)?;
                        Ok(quote! { #p => #b })
                    })
                    .collect::<Result<_>>()?;
                Ok(quote! { match #scrut { #(#arm_tokens),* } })
            }
            IrExpr::Block(exprs) => {
                let expr_tokens: Vec<TokenStream> = exprs.iter()
                    .map(|e| self.expr_to_tokens(e))
                    .collect::<Result<_>>()?;
                Ok(quote! { { #(#expr_tokens;)* } })
            }
            IrExpr::Return(expr) => {
                let e = self.expr_to_tokens(expr)?;
                Ok(quote! { return #e })
            }

            // === Assignment ===
            IrExpr::Assign { target, value } => {
                let t = format_ident!("{}", target);
                let v = self.expr_to_tokens(value)?;
                Ok(quote! { #t = #v })
            }
            IrExpr::Let { name, value, body } => {
                let n = format_ident!("{}", name);
                let v = self.expr_to_tokens(value)?;
                let b = self.expr_to_tokens(body)?;
                Ok(quote! { { let #n = #v; #b } })
            }

            // === Functions ===
            IrExpr::Lambda { params, body } => {
                let param_idents: Vec<_> = params.iter()
                    .map(|p| format_ident!("{}", p))
                    .collect();
                let body_tokens: Vec<TokenStream> = body.iter()
                    .map(|e| self.expr_to_tokens(e))
                    .collect::<Result<_>>()?;
                Ok(quote! { |#(#param_idents),*| { #(#body_tokens;)* } })
            }

            // === Lisp-specific (generate as comments or runtime calls) ===
            IrExpr::Quote(expr) => {
                let e = self.expr_to_tokens(expr)?;
                Ok(quote! { /* quote */ #e })
            }
            IrExpr::Quasiquote(expr) => {
                let e = self.expr_to_tokens(expr)?;
                Ok(quote! { /* quasiquote */ #e })
            }
            IrExpr::Unquote(expr) => {
                let e = self.expr_to_tokens(expr)?;
                Ok(quote! { #e })
            }

            // === Logic/AI specific (generate runtime calls) ===
            IrExpr::Goal { pattern, body } => {
                let p = self.expr_to_tokens(pattern)?;
                let b: Vec<TokenStream> = body.iter()
                    .map(|e| self.expr_to_tokens(e))
                    .collect::<Result<_>>()?;
                Ok(quote! { goal(#p, || { #(#b;)* }) })
            }
            IrExpr::Unify { left, right } => {
                let l = self.expr_to_tokens(left)?;
                let r = self.expr_to_tokens(right)?;
                Ok(quote! { unify(#l, #r) })
            }
            IrExpr::PatternMatch { value, pattern } => {
                let v = self.expr_to_tokens(value)?;
                let p = self.expr_to_tokens(pattern)?;
                Ok(quote! { matches!(#v, #p) })
            }
            IrExpr::Choice(choices) => {
                let choice_tokens: Vec<TokenStream> = choices.iter()
                    .map(|c| self.expr_to_tokens(c))
                    .collect::<Result<_>>()?;
                Ok(quote! { choice([#(|| #choice_tokens),*]) })
            }

            // === Production systems (OPS5) ===
            IrExpr::WmeCreate { class, attributes } => {
                let c = format_ident!("{}", class);
                let attrs: Vec<TokenStream> = attributes.iter()
                    .map(|(k, v)| {
                        let ki = format_ident!("{}", k);
                        let vi = self.expr_to_tokens(v)?;
                        Ok(quote! { #ki: #vi })
                    })
                    .collect::<Result<_>>()?;
                Ok(quote! { wm.insert(#c { #(#attrs),* }) })
            }

            // === Comments ===
            IrExpr::Comment(text) => {
                Ok(quote! { /* #text */ })
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

    /// Convert string binary operator to tokens
    fn string_binop_to_tokens(&self, op: &str) -> TokenStream {
        match op {
            "+" | "add" => quote! { + },
            "-" | "sub" => quote! { - },
            "*" | "mul" => quote! { * },
            "/" | "div" => quote! { / },
            "%" | "mod" | "rem" => quote! { % },
            "==" | "eq" => quote! { == },
            "!=" | "ne" | "<>" => quote! { != },
            "<" | "lt" => quote! { < },
            "<=" | "le" => quote! { <= },
            ">" | "gt" => quote! { > },
            ">=" | "ge" => quote! { >= },
            "&&" | "and" => quote! { && },
            "||" | "or" => quote! { || },
            "^" | "xor" => quote! { ^ },
            "&" | "bitand" => quote! { & },
            "|" | "bitor" => quote! { | },
            "<<" | "shl" => quote! { << },
            ">>" | "shr" => quote! { >> },
            "**" | "pow" => quote! { .pow },
            _ => {
                let op_ident = format_ident!("{}", op);
                quote! { .#op_ident() }
            }
        }
    }

    /// Convert string unary operator to tokens
    fn string_unaryop_to_tokens(&self, op: &str) -> TokenStream {
        match op {
            "-" | "neg" => quote! { - },
            "!" | "not" => quote! { ! },
            "~" | "bitnot" => quote! { ! },
            "*" | "deref" => quote! { * },
            "&" | "ref" => quote! { & },
            _ => quote! { /* unknown unary op */ }
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
        let mut builder = IrBuilder::with_language("test", SourceLanguage::Fortran77);
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
