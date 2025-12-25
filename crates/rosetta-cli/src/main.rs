//! # Rosetta CLI
//!
//! Command-line interface for the Rosetta transpiler.
//!
//! ## Usage
//!
//! ```bash
//! # Transpile FORTRAN to Rust
//! rosetta transpile program.f90 -o program.rs
//!
//! # Specify source language
//! rosetta transpile --lang fortran77 legacy.f -o modern.rs
//!
//! # Validate transpilation (compare outputs)
//! rosetta validate program.f90 program.rs --input test_data.txt
//! ```

use clap::{Parser, Subcommand};
use colored::Colorize;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "rosetta")]
#[command(author = "Yatrogenesis")]
#[command(version = "0.1.0")]
#[command(about = "Legacy language to Rust transpiler", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Transpile source code to Rust
    Transpile {
        /// Input file
        input: PathBuf,

        /// Output file
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Source language (fortran77, fortran90, cobol, lisp, quickbasic, ml)
        #[arg(short, long)]
        lang: Option<String>,

        /// Generate unsafe Rust for exact equivalence
        #[arg(long)]
        unsafe_mode: bool,

        /// Include original source as comments
        #[arg(long)]
        include_comments: bool,
    },

    /// Validate transpilation by comparing outputs
    Validate {
        /// Original source file
        original: PathBuf,

        /// Transpiled Rust file
        transpiled: PathBuf,

        /// Input data for comparison
        #[arg(long)]
        input: Option<PathBuf>,

        /// Tolerance for numerical comparison
        #[arg(long, default_value = "1e-10")]
        tolerance: f64,
    },

    /// List supported languages
    Languages,

    /// Show information about a source file
    Info {
        /// Source file to analyze
        file: PathBuf,
    },
}

fn main() -> anyhow::Result<()> {
    // Initialize logging
    tracing_subscriber::fmt::init();

    let cli = Cli::parse();

    match cli.command {
        Commands::Transpile { input, output, lang, unsafe_mode, include_comments } => {
            println!("{} {}", "Transpiling:".green().bold(), input.display());

            // Detect language from extension if not specified
            let language = lang.unwrap_or_else(|| {
                detect_language(&input).unwrap_or_else(|| "fortran90".to_string())
            });

            println!("  Language: {}", language.cyan());

            let output_path = output.unwrap_or_else(|| {
                input.with_extension("rs")
            });

            println!("  Output: {}", output_path.display().to_string().cyan());

            // TODO: Implement actual transpilation
            println!("{}", "Transpilation not yet implemented".yellow());

            Ok(())
        }

        Commands::Validate { original, transpiled, input, tolerance } => {
            println!("{}", "Validating transpilation...".green().bold());
            println!("  Original: {}", original.display());
            println!("  Transpiled: {}", transpiled.display());
            println!("  Tolerance: {}", tolerance);

            if let Some(ref inp) = input {
                println!("  Input data: {}", inp.display());
            }

            // TODO: Implement validation
            println!("{}", "Validation not yet implemented".yellow());

            Ok(())
        }

        Commands::Languages => {
            println!("{}", "Supported Languages:".green().bold());
            println!();
            println!("  {} - FORTRAN 77 (fixed format)", "fortran77".cyan());
            println!("  {} - FORTRAN 90 (free format)", "fortran90".cyan());
            println!("  {} - FORTRAN 95", "fortran95".cyan());
            println!("  {} - FORTRAN 2003", "fortran2003".cyan());
            println!("  {} - FORTRAN 2008", "fortran2008".cyan());
            println!("  {} - COBOL", "cobol".cyan());
            println!("  {} - Common Lisp", "lisp".cyan());
            println!("  {} - Scheme", "scheme".cyan());
            println!("  {} - QuickBASIC/QBASIC", "quickbasic".cyan());
            println!("  {} - Standard ML", "sml".cyan());
            println!("  {} - OCaml", "ocaml".cyan());

            Ok(())
        }

        Commands::Info { file } => {
            println!("{} {}", "Analyzing:".green().bold(), file.display());

            let language = detect_language(&file);
            if let Some(lang) = language {
                println!("  Detected language: {}", lang.cyan());
            } else {
                println!("  {}", "Unknown file type".yellow());
            }

            // TODO: Show more file info (LOC, complexity, etc.)

            Ok(())
        }
    }
}

fn detect_language(path: &PathBuf) -> Option<String> {
    let ext = path.extension()?.to_str()?;

    match ext.to_lowercase().as_str() {
        "f" | "for" | "f77" => Some("fortran77".to_string()),
        "f90" => Some("fortran90".to_string()),
        "f95" => Some("fortran95".to_string()),
        "f03" => Some("fortran2003".to_string()),
        "f08" => Some("fortran2008".to_string()),
        "cob" | "cbl" => Some("cobol".to_string()),
        "lisp" | "cl" | "lsp" => Some("lisp".to_string()),
        "scm" | "ss" => Some("scheme".to_string()),
        "bas" | "bi" => Some("quickbasic".to_string()),
        "sml" | "sig" => Some("sml".to_string()),
        "ml" | "mli" => Some("ocaml".to_string()),
        _ => None,
    }
}
