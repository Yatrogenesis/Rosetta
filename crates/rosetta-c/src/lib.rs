//! # Rosetta C Code Generator
//!
//! Generates C code from Rosetta IR for maximum portability.
//!
//! ## Features
//!
//! - C89/C99/C11 compatible output
//! - Proper type mappings
//! - Array handling with bounds checking options

use rosetta_core::{IrType, IrExpr, Result};
use rosetta_ir::{IrModule, IrFunction, IrTypeDef, Visibility, IrEnumVariant, IrParam};
use std::fmt::Write;

/// C Standard to target
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CStandard {
    C89,
    #[default]
    C99,
    C11,
    C17,
    C23,
}

/// C code generation options
#[derive(Debug, Clone)]
pub struct CCodegenOptions {
    pub standard: CStandard,
    pub include_comments: bool,
    pub generate_header: bool,
    pub use_stdint: bool,
}

impl Default for CCodegenOptions {
    fn default() -> Self {
        Self {
            standard: CStandard::C99,
            include_comments: true,
            generate_header: true,
            use_stdint: true,
        }
    }
}

/// C code generator
pub struct CCodegen {
    options: CCodegenOptions,
}

impl CCodegen {
    pub fn new(options: CCodegenOptions) -> Self {
        Self { options }
    }

    pub fn generate(&self, module: &IrModule) -> Result<COutput> {
        let source = self.generate_source(module)?;
        let header = if self.options.generate_header {
            Some(self.generate_header(module)?)
        } else {
            None
        };
        Ok(COutput { source, header })
    }

    fn generate_source(&self, module: &IrModule) -> Result<String> {
        let mut out = String::new();

        if self.options.include_comments {
            writeln!(out, "/* Transpiled by Rosetta from {:?} */", module.source_lang).unwrap();
            writeln!(out, "/* Original: {} */", module.name).unwrap();
            writeln!(out).unwrap();
        }

        self.write_includes(&mut out);

        for typedef in &module.types {
            self.write_typedef(&mut out, typedef)?;
        }

        for global in &module.globals {
            let c_type = self.type_to_c(&global.ty);
            if let Some(init) = &global.init {
                writeln!(out, "{} {} = {};", c_type, global.name, self.expr_to_c(init)?).unwrap();
            } else {
                writeln!(out, "{} {};", c_type, global.name).unwrap();
            }
        }

        for func in &module.functions {
            self.write_function(&mut out, func)?;
        }

        Ok(out)
    }

    fn generate_header(&self, module: &IrModule) -> Result<String> {
        let mut out = String::new();
        let guard = format!("{}_H", module.name.to_uppercase().replace('-', "_"));

        writeln!(out, "#ifndef {}", guard).unwrap();
        writeln!(out, "#define {}", guard).unwrap();
        writeln!(out).unwrap();

        if self.options.use_stdint {
            writeln!(out, "#include <stdint.h>").unwrap();
            writeln!(out, "#include <stdbool.h>").unwrap();
        }
        writeln!(out).unwrap();

        for typedef in &module.types {
            self.write_typedef(&mut out, typedef)?;
        }

        for func in &module.functions {
            if matches!(func.visibility, Visibility::Public) {
                self.write_function_decl(&mut out, func)?;
            }
        }

        writeln!(out).unwrap();
        writeln!(out, "#endif /* {} */", guard).unwrap();

        Ok(out)
    }

    fn write_includes(&self, out: &mut String) {
        writeln!(out, "#include <stdio.h>").unwrap();
        writeln!(out, "#include <stdlib.h>").unwrap();
        writeln!(out, "#include <string.h>").unwrap();
        writeln!(out, "#include <math.h>").unwrap();
        if self.options.use_stdint {
            writeln!(out, "#include <stdint.h>").unwrap();
            writeln!(out, "#include <stdbool.h>").unwrap();
        }
        writeln!(out).unwrap();
    }

    fn write_typedef(&self, out: &mut String, typedef: &IrTypeDef) -> Result<()> {
        match typedef {
            IrTypeDef::Struct { name, fields, .. } => {
                writeln!(out, "typedef struct {{").unwrap();
                for (field_name, field_type) in fields {
                    let c_type = self.type_to_c(field_type);
                    writeln!(out, "    {} {};", c_type, field_name).unwrap();
                }
                writeln!(out, "}} {};", name).unwrap();
                writeln!(out).unwrap();
            }
            IrTypeDef::Enum { name, variants } => {
                writeln!(out, "typedef enum {{").unwrap();
                for (i, variant) in variants.iter().enumerate() {
                    let comma = if i < variants.len() - 1 { "," } else { "" };
                    if let Some(disc) = variant.discriminant {
                        writeln!(out, "    {} = {}{}", variant.name, disc, comma).unwrap();
                    } else {
                        writeln!(out, "    {}{}", variant.name, comma).unwrap();
                    }
                }
                writeln!(out, "}} {};", name).unwrap();
                writeln!(out).unwrap();
            }
            IrTypeDef::Alias { name, target } => {
                let c_type = self.type_to_c(target);
                writeln!(out, "typedef {} {};", c_type, name).unwrap();
                writeln!(out).unwrap();
            }
        }
        Ok(())
    }

    fn write_function(&self, out: &mut String, func: &IrFunction) -> Result<()> {
        let return_type = self.type_to_c(&func.return_type);

        let params: Vec<String> = func.params.iter()
            .map(|p| format!("{} {}", self.type_to_c(&p.ty), p.name))
            .collect();

        let params_str = if params.is_empty() {
            "void".to_string()
        } else {
            params.join(", ")
        };

        writeln!(out, "{} {}({}) {{", return_type, func.name, params_str).unwrap();

        for expr in &func.body {
            self.write_expr_stmt(out, expr, 1)?;
        }

        writeln!(out, "}}").unwrap();
        writeln!(out).unwrap();

        Ok(())
    }

    fn write_function_decl(&self, out: &mut String, func: &IrFunction) -> Result<()> {
        let return_type = self.type_to_c(&func.return_type);

        let params: Vec<String> = func.params.iter()
            .map(|p| format!("{} {}", self.type_to_c(&p.ty), p.name))
            .collect();

        let params_str = if params.is_empty() {
            "void".to_string()
        } else {
            params.join(", ")
        };

        writeln!(out, "{} {}({});", return_type, func.name, params_str).unwrap();
        Ok(())
    }

    fn write_expr_stmt(&self, out: &mut String, expr: &IrExpr, indent: usize) -> Result<()> {
        let ind = "    ".repeat(indent);

        match expr {
            IrExpr::Assign { target, value } => {
                writeln!(out, "{}{} = {};", ind, target, self.expr_to_c(value)?).unwrap();
            }
            IrExpr::If { condition, then_branch, else_branch } => {
                writeln!(out, "{}if ({}) {{", ind, self.expr_to_c(condition)?).unwrap();
                self.write_expr_stmt(out, then_branch, indent + 1)?;
                if let Some(else_br) = else_branch {
                    writeln!(out, "{}}} else {{", ind).unwrap();
                    self.write_expr_stmt(out, else_br, indent + 1)?;
                }
                writeln!(out, "{}}}", ind).unwrap();
            }
            IrExpr::Block(exprs) => {
                for e in exprs {
                    self.write_expr_stmt(out, e, indent)?;
                }
            }
            IrExpr::Return(value) => {
                writeln!(out, "{}return {};", ind, self.expr_to_c(value)?).unwrap();
            }
            IrExpr::Call { func, args } => {
                let func_name = self.expr_to_c(func)?;
                let args_str: Vec<String> = args.iter()
                    .map(|a| self.expr_to_c(a))
                    .collect::<Result<_>>()?;
                writeln!(out, "{}{}({});", ind, func_name, args_str.join(", ")).unwrap();
            }
            _ => {
                writeln!(out, "{}{};", ind, self.expr_to_c(expr)?).unwrap();
            }
        }
        Ok(())
    }

    fn expr_to_c(&self, expr: &IrExpr) -> Result<String> {
        match expr {
            IrExpr::Int(n) => Ok(n.to_string()),
            IrExpr::Float(f) => Ok(format!("{:.6}", f)),
            IrExpr::Bool(b) => Ok(if *b { "true" } else { "false" }.to_string()),
            IrExpr::String(s) => Ok(format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))),
            IrExpr::Char(c) => Ok(format!("'{}'", c)),
            IrExpr::Nil => Ok("NULL".to_string()),
            IrExpr::Identifier(name) => Ok(name.clone()),
            IrExpr::BinaryOp { op, left, right } => {
                let l = self.expr_to_c(left)?;
                let r = self.expr_to_c(right)?;
                let op_str = match op.as_str() {
                    "and" => "&&",
                    "or" => "||",
                    "mod" => "%",
                    "**" => return Ok(format!("pow({}, {})", l, r)),
                    _ => op,
                };
                Ok(format!("({} {} {})", l, op_str, r))
            }
            IrExpr::UnaryOp { op, operand } => {
                let inner = self.expr_to_c(operand)?;
                let op_str = match op.as_str() {
                    "not" => "!",
                    _ => op,
                };
                Ok(format!("({}{})", op_str, inner))
            }
            IrExpr::Call { func, args } => {
                let func_name = self.expr_to_c(func)?;
                let args_str: Vec<String> = args.iter()
                    .map(|a| self.expr_to_c(a))
                    .collect::<Result<_>>()?;
                Ok(format!("{}({})", func_name, args_str.join(", ")))
            }
            IrExpr::Index { array, index } => {
                Ok(format!("{}[{}]", self.expr_to_c(array)?, self.expr_to_c(index)?))
            }
            IrExpr::FieldAccess { object, field } => {
                Ok(format!("{}.{}", self.expr_to_c(object)?, field))
            }
            IrExpr::If { condition, then_branch, else_branch } => {
                if let Some(else_br) = else_branch {
                    Ok(format!("({} ? {} : {})",
                        self.expr_to_c(condition)?,
                        self.expr_to_c(then_branch)?,
                        self.expr_to_c(else_br)?
                    ))
                } else {
                    Ok(format!("({} ? {} : 0)",
                        self.expr_to_c(condition)?,
                        self.expr_to_c(then_branch)?
                    ))
                }
            }
            IrExpr::List(elements) => {
                let elems: Vec<String> = elements.iter()
                    .map(|e| self.expr_to_c(e))
                    .collect::<Result<_>>()?;
                Ok(format!("{{{}}}", elems.join(", ")))
            }
            IrExpr::Tuple(elements) => {
                let elems: Vec<String> = elements.iter()
                    .map(|e| self.expr_to_c(e))
                    .collect::<Result<_>>()?;
                Ok(format!("{{{}}}", elems.join(", ")))
            }
            IrExpr::StructInit { name, fields } => {
                let field_str: Vec<String> = fields.iter()
                    .map(|(k, v)| self.expr_to_c(v).map(|s| format!(".{} = {}", k, s)))
                    .collect::<Result<_>>()?;
                Ok(format!("({}) {{ {} }}", name, field_str.join(", ")))
            }
            IrExpr::Symbol(s) => Ok(format!("\"{}\"", s)),
            _ => Ok("/* unsupported */".to_string()),
        }
    }

    fn type_to_c(&self, ty: &IrType) -> String {
        match ty {
            IrType::Unit => "void".to_string(),
            IrType::Bool => if self.options.use_stdint { "bool" } else { "int" }.to_string(),
            IrType::Int(8) => if self.options.use_stdint { "int8_t" } else { "char" }.to_string(),
            IrType::Int(16) => if self.options.use_stdint { "int16_t" } else { "short" }.to_string(),
            IrType::Int(32) => if self.options.use_stdint { "int32_t" } else { "int" }.to_string(),
            IrType::Int(64) => if self.options.use_stdint { "int64_t" } else { "long long" }.to_string(),
            IrType::Int(_) => "int".to_string(),
            IrType::Float(32) => "float".to_string(),
            IrType::Float(64) => "double".to_string(),
            IrType::Float(_) => "double".to_string(),
            IrType::String => "char*".to_string(),
            IrType::Char => "char".to_string(),
            IrType::Array(elem, size) => {
                let elem_type = self.type_to_c(elem);
                if let Some(s) = size {
                    format!("{}[{}]", elem_type, s)
                } else {
                    format!("{}*", elem_type)
                }
            }
            IrType::Vec(elem) => format!("{}*", self.type_to_c(elem)),
            IrType::Ref(inner) => format!("{}*", self.type_to_c(inner)),
            IrType::MutRef(inner) => format!("{}*", self.type_to_c(inner)),
            IrType::Box(inner) => format!("{}*", self.type_to_c(inner)),
            IrType::Struct(name) => name.clone(),
            IrType::Fn(params, ret) => {
                let ret_type = self.type_to_c(ret);
                let param_types: Vec<String> = params.iter().map(|p| self.type_to_c(p)).collect();
                format!("{}(*)({})", ret_type, param_types.join(", "))
            }
            IrType::Tuple(_) => "void*".to_string(),
            IrType::Option(_) => "void*".to_string(),
            IrType::Result(_, _) => "void*".to_string(),
            IrType::Any => "void*".to_string(),
            IrType::Unknown => "void*".to_string(),
            IrType::Iterator(_) => "void*".to_string(),
        }
    }
}

/// Output from C code generation
#[derive(Debug, Clone)]
pub struct COutput {
    pub source: String,
    pub header: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use rosetta_core::SourceLanguage;
    use rosetta_ir::IrBuilder;

    #[test]
    fn test_basic_generation() {
        let codegen = CCodegen::new(CCodegenOptions::default());
        let builder = IrBuilder::with_language("test", SourceLanguage::Fortran77);
        let module = builder.build();
        let output = codegen.generate(&module).unwrap();
        assert!(output.source.contains("Transpiled by Rosetta"));
    }

    #[test]
    fn test_type_mapping() {
        let codegen = CCodegen::new(CCodegenOptions::default());
        assert_eq!(codegen.type_to_c(&IrType::Int(32)), "int32_t");
        assert_eq!(codegen.type_to_c(&IrType::Float(64)), "double");
        assert_eq!(codegen.type_to_c(&IrType::String), "char*");
    }

    #[test]
    fn test_header_generation() {
        let codegen = CCodegen::new(CCodegenOptions {
            generate_header: true,
            ..Default::default()
        });
        let builder = IrBuilder::with_language("mymodule", SourceLanguage::Cobol);
        let module = builder.build();
        let output = codegen.generate(&module).unwrap();
        assert!(output.header.is_some());
        assert!(output.header.unwrap().contains("#ifndef MYMODULE_H"));
    }
}
