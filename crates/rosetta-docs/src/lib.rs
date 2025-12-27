//! # Rosetta Documentation Generator
//!
//! Generates comprehensive documentation for transpiled code.
//!
//! ## Features
//!
//! - Markdown documentation generation
//! - HTML output with syntax highlighting
//! - Cross-reference tables (original vs transpiled)
//! - Type mapping documentation
//! - Function signature comparison

use rosetta_core::{IrType, Result, SourceLanguage};
use rosetta_ir::{IrModule, IrFunction, IrTypeDef, Visibility};
use std::fmt::Write;

/// Documentation output format
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DocFormat {
    #[default]
    Markdown,
    Html,
    Json,
}

/// Documentation generation options
#[derive(Debug, Clone)]
pub struct DocOptions {
    pub format: DocFormat,
    pub include_ir: bool,
    pub include_type_mappings: bool,
    pub include_examples: bool,
    pub syntax_highlight: bool,
    pub project_name: Option<String>,
}

impl Default for DocOptions {
    fn default() -> Self {
        Self {
            format: DocFormat::Markdown,
            include_ir: true,
            include_type_mappings: true,
            include_examples: true,
            syntax_highlight: true,
            project_name: None,
        }
    }
}

/// Documentation generator
pub struct DocGenerator {
    options: DocOptions,
}

impl DocGenerator {
    pub fn new(options: DocOptions) -> Self {
        Self { options }
    }

    /// Generate documentation for a module
    pub fn generate(&self, module: &IrModule) -> Result<String> {
        match self.options.format {
            DocFormat::Markdown => self.generate_markdown(module),
            DocFormat::Html => self.generate_html(module),
            DocFormat::Json => self.generate_json(module),
        }
    }

    fn generate_markdown(&self, module: &IrModule) -> Result<String> {
        let mut out = String::new();

        // Title
        let title = self.options.project_name.as_deref().unwrap_or(&module.name);
        writeln!(out, "# {}", title).unwrap();
        writeln!(out).unwrap();

        // Overview
        writeln!(out, "## Overview").unwrap();
        writeln!(out).unwrap();
        writeln!(out, "Transpiled from **{:?}** using Rosetta.", module.source_lang).unwrap();
        writeln!(out).unwrap();

        // Statistics
        writeln!(out, "### Statistics").unwrap();
        writeln!(out).unwrap();
        writeln!(out, "| Metric | Count |").unwrap();
        writeln!(out, "|--------|-------|").unwrap();
        writeln!(out, "| Functions | {} |", module.functions.len()).unwrap();
        writeln!(out, "| Types | {} |", module.types.len()).unwrap();
        writeln!(out, "| Globals | {} |", module.globals.len()).unwrap();
        writeln!(out).unwrap();

        // Type definitions
        if !module.types.is_empty() {
            writeln!(out, "## Types").unwrap();
            writeln!(out).unwrap();

            for typedef in &module.types {
                self.write_typedef_doc(&mut out, typedef)?;
            }
        }

        // Functions
        if !module.functions.is_empty() {
            writeln!(out, "## Functions").unwrap();
            writeln!(out).unwrap();

            // Table of contents
            writeln!(out, "### Index").unwrap();
            writeln!(out).unwrap();
            for func in &module.functions {
                let visibility = match func.visibility {
                    Visibility::Public => "pub",
                    Visibility::Private => "priv",
                    Visibility::Crate => "crate",
                    Visibility::Super => "super",
                };
                writeln!(out, "- [`{}`](#{}) ({})", func.name, func.name.to_lowercase(), visibility).unwrap();
            }
            writeln!(out).unwrap();

            // Detailed documentation
            for func in &module.functions {
                self.write_function_doc(&mut out, func, module.source_lang)?;
            }
        }

        // Type mappings
        if self.options.include_type_mappings {
            writeln!(out, "## Type Mappings").unwrap();
            writeln!(out).unwrap();
            self.write_type_mappings(&mut out, module.source_lang)?;
        }

        Ok(out)
    }

    fn write_typedef_doc(&self, out: &mut String, typedef: &IrTypeDef) -> Result<()> {
        match typedef {
            IrTypeDef::Struct { name, fields, .. } => {
                writeln!(out, "### struct `{}`", name).unwrap();
                writeln!(out).unwrap();
                writeln!(out, "| Field | Type |").unwrap();
                writeln!(out, "|-------|------|").unwrap();
                for (field_name, field_type) in fields {
                    writeln!(out, "| `{}` | `{}` |", field_name, self.type_to_string(field_type)).unwrap();
                }
                writeln!(out).unwrap();
            }
            IrTypeDef::Enum { name, variants } => {
                writeln!(out, "### enum `{}`", name).unwrap();
                writeln!(out).unwrap();
                writeln!(out, "| Variant | Value |").unwrap();
                writeln!(out, "|---------|-------|").unwrap();
                for variant in variants {
                    let disc = variant.discriminant
                        .map(|d| d.to_string())
                        .unwrap_or_else(|| "auto".to_string());
                    writeln!(out, "| `{}` | {} |", variant.name, disc).unwrap();
                }
                writeln!(out).unwrap();
            }
            IrTypeDef::Alias { name, target } => {
                writeln!(out, "### type `{}`", name).unwrap();
                writeln!(out).unwrap();
                writeln!(out, "Alias for `{}`", self.type_to_string(target)).unwrap();
                writeln!(out).unwrap();
            }
        }
        Ok(())
    }

    fn write_function_doc(&self, out: &mut String, func: &IrFunction, source: SourceLanguage) -> Result<()> {
        writeln!(out, "### `{}`", func.name).unwrap();
        writeln!(out).unwrap();

        // Signature
        let params: Vec<String> = func.params.iter()
            .map(|p| format!("{}: {}", p.name, self.type_to_string(&p.ty)))
            .collect();

        let ret = if matches!(func.return_type, IrType::Unit) {
            String::new()
        } else {
            format!(" -> {}", self.type_to_string(&func.return_type))
        };

        let visibility = match func.visibility {
            Visibility::Public => "pub ",
            Visibility::Crate => "pub(crate) ",
            Visibility::Super => "pub(super) ",
            Visibility::Private => "",
        };

        writeln!(out, "```rust").unwrap();
        writeln!(out, "{}fn {}({}){}", visibility, func.name, params.join(", "), ret).unwrap();
        writeln!(out, "```").unwrap();
        writeln!(out).unwrap();

        // Parameters
        if !func.params.is_empty() {
            writeln!(out, "**Parameters:**").unwrap();
            writeln!(out).unwrap();
            for param in &func.params {
                let mutability = if param.is_mutable { " (mutable)" } else { "" };
                let by_ref = if param.by_ref { " (by reference)" } else { "" };
                writeln!(out, "- `{}`: `{}`{}{}",
                    param.name,
                    self.type_to_string(&param.ty),
                    mutability,
                    by_ref
                ).unwrap();
            }
            writeln!(out).unwrap();
        }

        // Return type
        if !matches!(func.return_type, IrType::Unit) {
            writeln!(out, "**Returns:** `{}`", self.type_to_string(&func.return_type)).unwrap();
            writeln!(out).unwrap();
        }

        // Source language info
        writeln!(out, "**Original:** {:?}", source).unwrap();
        writeln!(out).unwrap();

        writeln!(out, "---").unwrap();
        writeln!(out).unwrap();

        Ok(())
    }

    fn write_type_mappings(&self, out: &mut String, source: SourceLanguage) -> Result<()> {
        writeln!(out, "### {} to Rust", format!("{:?}", source)).unwrap();
        writeln!(out).unwrap();
        writeln!(out, "| Source Type | Rust Type |").unwrap();
        writeln!(out, "|-------------|-----------|").unwrap();

        let mappings = self.get_type_mappings(source);
        for (src, dst) in mappings {
            writeln!(out, "| `{}` | `{}` |", src, dst).unwrap();
        }
        writeln!(out).unwrap();

        Ok(())
    }

    fn get_type_mappings(&self, source: SourceLanguage) -> Vec<(&'static str, &'static str)> {
        match source {
            SourceLanguage::Fortran77 | SourceLanguage::Fortran90 => vec![
                ("INTEGER", "i32"),
                ("INTEGER*2", "i16"),
                ("INTEGER*4", "i32"),
                ("INTEGER*8", "i64"),
                ("REAL", "f32"),
                ("REAL*4", "f32"),
                ("REAL*8 / DOUBLE PRECISION", "f64"),
                ("COMPLEX", "(f32, f32)"),
                ("COMPLEX*16", "(f64, f64)"),
                ("LOGICAL", "bool"),
                ("CHARACTER", "char"),
                ("CHARACTER*n", "String"),
            ],
            SourceLanguage::Cobol => vec![
                ("PIC 9", "u32"),
                ("PIC S9", "i32"),
                ("PIC 9V9", "f64"),
                ("PIC X", "char"),
                ("PIC X(n)", "String"),
                ("COMP / BINARY", "i32"),
                ("COMP-1", "f32"),
                ("COMP-2", "f64"),
                ("COMP-3 / PACKED-DECIMAL", "Decimal"),
            ],
            SourceLanguage::CommonLisp => vec![
                ("fixnum", "i64"),
                ("bignum", "BigInt"),
                ("single-float", "f32"),
                ("double-float", "f64"),
                ("ratio", "Rational"),
                ("complex", "Complex<f64>"),
                ("character", "char"),
                ("string", "String"),
                ("symbol", "Symbol"),
                ("cons / list", "Vec<T>"),
                ("vector", "Vec<T>"),
                ("hash-table", "HashMap<K, V>"),
            ],
            SourceLanguage::Pascal => vec![
                ("Integer", "i32"),
                ("ShortInt", "i16"),
                ("LongInt", "i64"),
                ("Byte", "u8"),
                ("Word", "u16"),
                ("Real", "f64"),
                ("Single", "f32"),
                ("Double", "f64"),
                ("Boolean", "bool"),
                ("Char", "char"),
                ("String", "String"),
                ("Array", "Vec<T>"),
                ("Record", "struct"),
            ],
            _ => vec![
                ("integer", "i32"),
                ("float", "f64"),
                ("boolean", "bool"),
                ("character", "char"),
                ("string", "String"),
            ],
        }
    }

    fn generate_html(&self, module: &IrModule) -> Result<String> {
        let markdown = self.generate_markdown(module)?;

        let mut out = String::new();
        writeln!(out, "<!DOCTYPE html>").unwrap();
        writeln!(out, "<html lang=\"en\">").unwrap();
        writeln!(out, "<head>").unwrap();
        writeln!(out, "  <meta charset=\"UTF-8\">").unwrap();
        writeln!(out, "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">").unwrap();
        writeln!(out, "  <title>{} - Rosetta Documentation</title>", module.name).unwrap();
        writeln!(out, "  <style>").unwrap();
        writeln!(out, "    body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; max-width: 800px; margin: 0 auto; padding: 2rem; line-height: 1.6; }}").unwrap();
        writeln!(out, "    h1, h2, h3 {{ color: #333; }}").unwrap();
        writeln!(out, "    code {{ background: #f4f4f4; padding: 0.2em 0.4em; border-radius: 3px; font-family: 'Fira Code', monospace; }}").unwrap();
        writeln!(out, "    pre {{ background: #1e1e1e; color: #d4d4d4; padding: 1rem; border-radius: 5px; overflow-x: auto; }}").unwrap();
        writeln!(out, "    pre code {{ background: transparent; padding: 0; }}").unwrap();
        writeln!(out, "    table {{ border-collapse: collapse; width: 100%; margin: 1rem 0; }}").unwrap();
        writeln!(out, "    th, td {{ border: 1px solid #ddd; padding: 0.5rem; text-align: left; }}").unwrap();
        writeln!(out, "    th {{ background: #f8f8f8; }}").unwrap();
        writeln!(out, "    hr {{ border: none; border-top: 1px solid #eee; margin: 2rem 0; }}").unwrap();
        writeln!(out, "  </style>").unwrap();
        writeln!(out, "</head>").unwrap();
        writeln!(out, "<body>").unwrap();

        // Simple markdown to HTML conversion
        let html_content = self.markdown_to_html(&markdown);
        writeln!(out, "{}", html_content).unwrap();

        writeln!(out, "</body>").unwrap();
        writeln!(out, "</html>").unwrap();

        Ok(out)
    }

    fn markdown_to_html(&self, markdown: &str) -> String {
        let mut html = String::new();
        let mut in_code_block = false;
        let mut in_table = false;

        for line in markdown.lines() {
            if line.starts_with("```") {
                if in_code_block {
                    html.push_str("</code></pre>\n");
                    in_code_block = false;
                } else {
                    html.push_str("<pre><code>");
                    in_code_block = true;
                }
                continue;
            }

            if in_code_block {
                html.push_str(&line.replace('<', "&lt;").replace('>', "&gt;"));
                html.push('\n');
                continue;
            }

            // Tables
            if line.starts_with('|') {
                if !in_table {
                    html.push_str("<table>\n");
                    in_table = true;
                }
                if line.contains("---") {
                    continue; // Skip separator row
                }
                let cells: Vec<&str> = line.split('|')
                    .filter(|s| !s.is_empty())
                    .map(|s| s.trim())
                    .collect();
                html.push_str("<tr>");
                for cell in cells {
                    html.push_str(&format!("<td>{}</td>", self.inline_markdown(cell)));
                }
                html.push_str("</tr>\n");
                continue;
            } else if in_table {
                html.push_str("</table>\n");
                in_table = false;
            }

            // Headers
            if line.starts_with("### ") {
                html.push_str(&format!("<h3>{}</h3>\n", self.inline_markdown(&line[4..])));
            } else if line.starts_with("## ") {
                html.push_str(&format!("<h2>{}</h2>\n", self.inline_markdown(&line[3..])));
            } else if line.starts_with("# ") {
                html.push_str(&format!("<h1>{}</h1>\n", self.inline_markdown(&line[2..])));
            } else if line.starts_with("- ") {
                html.push_str(&format!("<li>{}</li>\n", self.inline_markdown(&line[2..])));
            } else if line.starts_with("---") {
                html.push_str("<hr>\n");
            } else if line.is_empty() {
                html.push_str("<br>\n");
            } else {
                html.push_str(&format!("<p>{}</p>\n", self.inline_markdown(line)));
            }
        }

        if in_table {
            html.push_str("</table>\n");
        }

        html
    }

    fn inline_markdown(&self, text: &str) -> String {
        let mut result = text.to_string();

        // Bold
        while let Some(start) = result.find("**") {
            if let Some(end) = result[start + 2..].find("**") {
                let content = &result[start + 2..start + 2 + end];
                result = format!(
                    "{}<strong>{}</strong>{}",
                    &result[..start],
                    content,
                    &result[start + 4 + end..]
                );
            } else {
                break;
            }
        }

        // Code
        while let Some(start) = result.find('`') {
            if let Some(end) = result[start + 1..].find('`') {
                let content = &result[start + 1..start + 1 + end];
                result = format!(
                    "{}<code>{}</code>{}",
                    &result[..start],
                    content,
                    &result[start + 2 + end..]
                );
            } else {
                break;
            }
        }

        result
    }

    fn generate_json(&self, module: &IrModule) -> Result<String> {
        let doc = DocJson {
            name: module.name.clone(),
            source_language: format!("{:?}", module.source_lang),
            functions: module.functions.iter().map(|f| FunctionDoc {
                name: f.name.clone(),
                visibility: format!("{:?}", f.visibility),
                params: f.params.iter().map(|p| ParamDoc {
                    name: p.name.clone(),
                    ty: self.type_to_string(&p.ty),
                    is_mutable: p.is_mutable,
                    by_ref: p.by_ref,
                }).collect(),
                return_type: self.type_to_string(&f.return_type),
            }).collect(),
            types: module.types.iter().map(|t| match t {
                IrTypeDef::Struct { name, fields, .. } => TypeDoc {
                    kind: "struct".to_string(),
                    name: name.clone(),
                    fields: Some(fields.iter().map(|(n, t)| (n.clone(), self.type_to_string(t))).collect()),
                    variants: None,
                    target: None,
                },
                IrTypeDef::Enum { name, variants } => TypeDoc {
                    kind: "enum".to_string(),
                    name: name.clone(),
                    fields: None,
                    variants: Some(variants.iter().map(|v| v.name.clone()).collect()),
                    target: None,
                },
                IrTypeDef::Alias { name, target } => TypeDoc {
                    kind: "alias".to_string(),
                    name: name.clone(),
                    fields: None,
                    variants: None,
                    target: Some(self.type_to_string(target)),
                },
            }).collect(),
        };

        serde_json::to_string_pretty(&doc)
            .map_err(|e| rosetta_core::TranspileError::SemanticError(format!("JSON error: {}", e)))
    }

    fn type_to_string(&self, ty: &IrType) -> String {
        match ty {
            IrType::Unit => "()".to_string(),
            IrType::Bool => "bool".to_string(),
            IrType::Int(8) => "i8".to_string(),
            IrType::Int(16) => "i16".to_string(),
            IrType::Int(32) => "i32".to_string(),
            IrType::Int(64) => "i64".to_string(),
            IrType::Int(n) => format!("i{}", n),
            IrType::Float(32) => "f32".to_string(),
            IrType::Float(64) => "f64".to_string(),
            IrType::Float(n) => format!("f{}", n),
            IrType::String => "String".to_string(),
            IrType::Char => "char".to_string(),
            IrType::Array(elem, Some(size)) => format!("[{}; {}]", self.type_to_string(elem), size),
            IrType::Array(elem, None) => format!("[{}]", self.type_to_string(elem)),
            IrType::Vec(elem) => format!("Vec<{}>", self.type_to_string(elem)),
            IrType::Ref(inner) => format!("&{}", self.type_to_string(inner)),
            IrType::MutRef(inner) => format!("&mut {}", self.type_to_string(inner)),
            IrType::Box(inner) => format!("Box<{}>", self.type_to_string(inner)),
            IrType::Option(inner) => format!("Option<{}>", self.type_to_string(inner)),
            IrType::Result(ok, err) => format!("Result<{}, {}>", self.type_to_string(ok), self.type_to_string(err)),
            IrType::Tuple(elems) => {
                let types: Vec<String> = elems.iter().map(|t| self.type_to_string(t)).collect();
                format!("({})", types.join(", "))
            }
            IrType::Fn(params, ret) => {
                let param_types: Vec<String> = params.iter().map(|t| self.type_to_string(t)).collect();
                format!("fn({}) -> {}", param_types.join(", "), self.type_to_string(ret))
            }
            IrType::Struct(name) => name.clone(),
            IrType::Any => "Any".to_string(),
            IrType::Unknown => "?".to_string(),
            IrType::Iterator(inner) => format!("Iterator<{}>", self.type_to_string(inner)),
        }
    }
}

#[derive(serde::Serialize)]
struct DocJson {
    name: String,
    source_language: String,
    functions: Vec<FunctionDoc>,
    types: Vec<TypeDoc>,
}

#[derive(serde::Serialize)]
struct FunctionDoc {
    name: String,
    visibility: String,
    params: Vec<ParamDoc>,
    return_type: String,
}

#[derive(serde::Serialize)]
struct ParamDoc {
    name: String,
    ty: String,
    is_mutable: bool,
    by_ref: bool,
}

#[derive(serde::Serialize)]
struct TypeDoc {
    kind: String,
    name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    fields: Option<Vec<(String, String)>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    variants: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    target: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use rosetta_ir::IrBuilder;

    #[test]
    fn test_markdown_generation() {
        let gen = DocGenerator::new(DocOptions::default());
        let builder = IrBuilder::with_language("test_module", SourceLanguage::Fortran77);
        let module = builder.build();
        let output = gen.generate(&module).unwrap();
        assert!(output.contains("# test_module"));
        assert!(output.contains("Fortran77"));
    }

    #[test]
    fn test_html_generation() {
        let gen = DocGenerator::new(DocOptions {
            format: DocFormat::Html,
            ..Default::default()
        });
        let builder = IrBuilder::with_language("html_test", SourceLanguage::Cobol);
        let module = builder.build();
        let output = gen.generate(&module).unwrap();
        assert!(output.contains("<!DOCTYPE html>"));
        assert!(output.contains("<title>html_test"));
    }

    #[test]
    fn test_json_generation() {
        let gen = DocGenerator::new(DocOptions {
            format: DocFormat::Json,
            ..Default::default()
        });
        let builder = IrBuilder::with_language("json_test", SourceLanguage::CommonLisp);
        let module = builder.build();
        let output = gen.generate(&module).unwrap();
        assert!(output.contains("\"name\": \"json_test\""));
    }
}
