//! # Rosetta WebAssembly Code Generator
//!
//! Generates WebAssembly (WAT text format) from Rosetta IR.
//!
//! ## Features
//!
//! - Generates WAT (WebAssembly Text) format
//! - Can be assembled to WASM with wat2wasm
//! - Supports i32, i64, f32, f64 value types

use rosetta_core::{IrType, IrExpr, Result};
use rosetta_ir::{IrModule, IrFunction, Visibility};
use std::fmt::Write;
use std::collections::HashMap;

/// WebAssembly code generation options
#[derive(Debug, Clone)]
pub struct WasmCodegenOptions {
    pub initial_memory_pages: u32,
    pub max_memory_pages: Option<u32>,
    pub export_memory: bool,
    pub include_comments: bool,
    pub wasi_compatible: bool,
}

impl Default for WasmCodegenOptions {
    fn default() -> Self {
        Self {
            initial_memory_pages: 1,
            max_memory_pages: Some(16),
            export_memory: true,
            include_comments: true,
            wasi_compatible: false,
        }
    }
}

/// WASM value types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WasmValType {
    I32,
    I64,
    F32,
    F64,
}

impl WasmValType {
    fn as_str(&self) -> &'static str {
        match self {
            WasmValType::I32 => "i32",
            WasmValType::I64 => "i64",
            WasmValType::F32 => "f32",
            WasmValType::F64 => "f64",
        }
    }
}

/// WebAssembly code generator
pub struct WasmCodegen {
    options: WasmCodegenOptions,
    local_indices: HashMap<String, u32>,
    label_counter: u32,
    data_offset: u32,
    string_data: Vec<(u32, String)>,
}

impl WasmCodegen {
    pub fn new(options: WasmCodegenOptions) -> Self {
        Self {
            options,
            local_indices: HashMap::new(),
            label_counter: 0,
            data_offset: 0,
            string_data: Vec::new(),
        }
    }

    pub fn generate(&mut self, module: &IrModule) -> Result<String> {
        let mut out = String::new();

        if self.options.include_comments {
            writeln!(out, ";; Transpiled by Rosetta from {:?}", module.source_lang).unwrap();
            writeln!(out, ";; Original: {}", module.name).unwrap();
        }

        writeln!(out, "(module ${}", module.name).unwrap();

        self.write_memory(&mut out);

        if self.options.wasi_compatible {
            self.write_wasi_imports(&mut out);
        }

        self.write_math_imports(&mut out);

        for func in &module.functions {
            self.write_function(&mut out, func)?;
        }

        self.write_data_section(&mut out)?;

        writeln!(out, ")").unwrap();

        Ok(out)
    }

    fn write_memory(&self, out: &mut String) {
        write!(out, "  (memory $memory {}", self.options.initial_memory_pages).unwrap();
        if let Some(max) = self.options.max_memory_pages {
            write!(out, " {}", max).unwrap();
        }
        writeln!(out, ")").unwrap();

        if self.options.export_memory {
            writeln!(out, "  (export \"memory\" (memory $memory))").unwrap();
        }
        writeln!(out).unwrap();
    }

    fn write_wasi_imports(&self, out: &mut String) {
        writeln!(out, "  ;; WASI imports").unwrap();
        writeln!(out, "  (import \"wasi_snapshot_preview1\" \"fd_write\"").unwrap();
        writeln!(out, "    (func $fd_write (param i32 i32 i32 i32) (result i32)))").unwrap();
        writeln!(out, "  (import \"wasi_snapshot_preview1\" \"fd_read\"").unwrap();
        writeln!(out, "    (func $fd_read (param i32 i32 i32 i32) (result i32)))").unwrap();
        writeln!(out, "  (import \"wasi_snapshot_preview1\" \"proc_exit\"").unwrap();
        writeln!(out, "    (func $proc_exit (param i32)))").unwrap();
        writeln!(out).unwrap();
    }

    fn write_math_imports(&self, out: &mut String) {
        writeln!(out, "  ;; Math function imports").unwrap();
        writeln!(out, "  (import \"math\" \"sin\" (func $sin (param f64) (result f64)))").unwrap();
        writeln!(out, "  (import \"math\" \"cos\" (func $cos (param f64) (result f64)))").unwrap();
        writeln!(out, "  (import \"math\" \"sqrt\" (func $sqrt (param f64) (result f64)))").unwrap();
        writeln!(out, "  (import \"math\" \"pow\" (func $pow (param f64 f64) (result f64)))").unwrap();
        writeln!(out, "  (import \"math\" \"log\" (func $log (param f64) (result f64)))").unwrap();
        writeln!(out, "  (import \"math\" \"exp\" (func $exp (param f64) (result f64)))").unwrap();
        writeln!(out).unwrap();
    }

    fn write_function(&mut self, out: &mut String, func: &IrFunction) -> Result<()> {
        self.local_indices.clear();
        self.label_counter = 0;

        let mut idx = 0u32;
        for param in &func.params {
            self.local_indices.insert(param.name.clone(), idx);
            idx += 1;
        }

        let params: Vec<String> = func.params.iter()
            .map(|p| format!("(param ${} {})", p.name, self.type_to_wasm(&p.ty).as_str()))
            .collect();

        let result = if matches!(func.return_type, IrType::Unit) {
            String::new()
        } else {
            format!(" (result {})", self.type_to_wasm(&func.return_type).as_str())
        };

        writeln!(out, "  (func ${} {}{}", func.name, params.join(" "), result).unwrap();

        for expr in &func.body {
            self.write_expr(out, expr, 2)?;
        }

        writeln!(out, "  )").unwrap();

        if matches!(func.visibility, Visibility::Public) {
            writeln!(out, "  (export \"{}\" (func ${}))", func.name, func.name).unwrap();
        }

        writeln!(out).unwrap();
        Ok(())
    }

    fn write_expr(&mut self, out: &mut String, expr: &IrExpr, indent: usize) -> Result<()> {
        let ind = "    ".repeat(indent);

        match expr {
            IrExpr::Int(n) => {
                writeln!(out, "{}i32.const {}", ind, n).unwrap();
            }
            IrExpr::Float(f) => {
                writeln!(out, "{}f64.const {}", ind, f).unwrap();
            }
            IrExpr::Bool(b) => {
                writeln!(out, "{}i32.const {}", ind, if *b { 1 } else { 0 }).unwrap();
            }
            IrExpr::Char(c) => {
                writeln!(out, "{}i32.const {}", ind, *c as u32).unwrap();
            }
            IrExpr::String(s) => {
                let offset = self.data_offset;
                self.string_data.push((offset, s.clone()));
                self.data_offset += s.len() as u32 + 1;
                writeln!(out, "{}i32.const {}", ind, offset).unwrap();
            }
            IrExpr::Nil => {
                writeln!(out, "{}i32.const 0", ind).unwrap();
            }
            IrExpr::Identifier(name) => {
                if self.local_indices.contains_key(name) {
                    writeln!(out, "{}local.get ${}", ind, name).unwrap();
                } else {
                    writeln!(out, "{}global.get ${}", ind, name).unwrap();
                }
            }
            IrExpr::BinaryOp { op, left, right } => {
                self.write_expr(out, left, indent)?;
                self.write_expr(out, right, indent)?;
                let wasm_op = match op.as_str() {
                    "+" => "i32.add",
                    "-" => "i32.sub",
                    "*" => "i32.mul",
                    "/" => "i32.div_s",
                    "%" | "mod" => "i32.rem_s",
                    "==" | "eq" => "i32.eq",
                    "!=" | "ne" => "i32.ne",
                    "<" | "lt" => "i32.lt_s",
                    "<=" | "le" => "i32.le_s",
                    ">" | "gt" => "i32.gt_s",
                    ">=" | "ge" => "i32.ge_s",
                    "and" | "&&" => "i32.and",
                    "or" | "||" => "i32.or",
                    "+." => "f64.add",
                    "-." => "f64.sub",
                    "*." => "f64.mul",
                    "/." => "f64.div",
                    _ => {
                        writeln!(out, "{};; unsupported op: {}", ind, op).unwrap();
                        return Ok(());
                    }
                };
                writeln!(out, "{}{}", ind, wasm_op).unwrap();
            }
            IrExpr::UnaryOp { op, operand } => {
                self.write_expr(out, operand, indent)?;
                match op.as_str() {
                    "-" => {
                        writeln!(out, "{}i32.const -1", ind).unwrap();
                        writeln!(out, "{}i32.mul", ind).unwrap();
                    }
                    "not" | "!" => {
                        writeln!(out, "{}i32.eqz", ind).unwrap();
                    }
                    _ => {
                        writeln!(out, "{};; unsupported unary op: {}", ind, op).unwrap();
                    }
                }
            }
            IrExpr::Call { func, args } => {
                for arg in args {
                    self.write_expr(out, arg, indent)?;
                }
                if let IrExpr::Identifier(name) = func.as_ref() {
                    writeln!(out, "{}call ${}", ind, name).unwrap();
                } else {
                    writeln!(out, "{};; indirect call not supported", ind).unwrap();
                }
            }
            IrExpr::Assign { target, value } => {
                self.write_expr(out, value, indent)?;
                if self.local_indices.contains_key(target) {
                    writeln!(out, "{}local.set ${}", ind, target).unwrap();
                } else {
                    writeln!(out, "{}global.set ${}", ind, target).unwrap();
                }
            }
            IrExpr::If { condition, then_branch, else_branch } => {
                self.write_expr(out, condition, indent)?;
                writeln!(out, "{}(if", ind).unwrap();
                writeln!(out, "{}  (then", ind).unwrap();
                self.write_expr(out, then_branch, indent + 2)?;
                writeln!(out, "{}  )", ind).unwrap();
                if let Some(else_br) = else_branch {
                    writeln!(out, "{}  (else", ind).unwrap();
                    self.write_expr(out, else_br, indent + 2)?;
                    writeln!(out, "{}  )", ind).unwrap();
                }
                writeln!(out, "{})", ind).unwrap();
            }
            IrExpr::Block(exprs) => {
                for e in exprs {
                    self.write_expr(out, e, indent)?;
                }
            }
            IrExpr::Return(value) => {
                self.write_expr(out, value, indent)?;
                writeln!(out, "{}return", ind).unwrap();
            }
            IrExpr::Index { array, index } => {
                self.write_expr(out, array, indent)?;
                self.write_expr(out, index, indent)?;
                writeln!(out, "{}i32.const 4", ind).unwrap();
                writeln!(out, "{}i32.mul", ind).unwrap();
                writeln!(out, "{}i32.add", ind).unwrap();
                writeln!(out, "{}i32.load", ind).unwrap();
            }
            _ => {
                writeln!(out, "{};; unsupported expression", ind).unwrap();
            }
        }
        Ok(())
    }

    fn write_data_section(&self, out: &mut String) -> Result<()> {
        if !self.string_data.is_empty() {
            writeln!(out, "  ;; Data section (strings)").unwrap();
            for (offset, data) in &self.string_data {
                let escaped = data.replace('\\', "\\\\").replace('"', "\\\"");
                writeln!(out, "  (data (i32.const {}) \"{}\\00\")", offset, escaped).unwrap();
            }
            writeln!(out).unwrap();
        }
        Ok(())
    }

    fn type_to_wasm(&self, ty: &IrType) -> WasmValType {
        match ty {
            IrType::Bool | IrType::Int(8) | IrType::Int(16) | IrType::Int(32) | IrType::Char => WasmValType::I32,
            IrType::Int(64) => WasmValType::I64,
            IrType::Float(32) => WasmValType::F32,
            IrType::Float(64) => WasmValType::F64,
            IrType::String | IrType::Ref(_) | IrType::MutRef(_) | IrType::Box(_) | IrType::Vec(_) => WasmValType::I32,
            IrType::Array(_, _) => WasmValType::I32,
            _ => WasmValType::I32,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rosetta_core::SourceLanguage;
    use rosetta_ir::IrBuilder;

    #[test]
    fn test_basic_generation() {
        let mut codegen = WasmCodegen::new(WasmCodegenOptions::default());
        let builder = IrBuilder::with_language("test", SourceLanguage::Fortran77);
        let module = builder.build();
        let output = codegen.generate(&module).unwrap();
        assert!(output.contains("(module $test"));
    }

    #[test]
    fn test_memory_config() {
        let mut codegen = WasmCodegen::new(WasmCodegenOptions {
            initial_memory_pages: 4,
            max_memory_pages: Some(32),
            ..Default::default()
        });
        let builder = IrBuilder::with_language("memtest", SourceLanguage::Cobol);
        let module = builder.build();
        let output = codegen.generate(&module).unwrap();
        assert!(output.contains("(memory $memory 4 32)"));
    }

    #[test]
    fn test_type_mapping() {
        let codegen = WasmCodegen::new(WasmCodegenOptions::default());
        assert_eq!(codegen.type_to_wasm(&IrType::Int(32)), WasmValType::I32);
        assert_eq!(codegen.type_to_wasm(&IrType::Float(64)), WasmValType::F64);
        assert_eq!(codegen.type_to_wasm(&IrType::String), WasmValType::I32);
    }

    #[test]
    fn test_wasi_mode() {
        let mut codegen = WasmCodegen::new(WasmCodegenOptions {
            wasi_compatible: true,
            ..Default::default()
        });
        let builder = IrBuilder::with_language("wasitest", SourceLanguage::Fortran77);
        let module = builder.build();
        let output = codegen.generate(&module).unwrap();
        assert!(output.contains("wasi_snapshot_preview1"));
    }
}
