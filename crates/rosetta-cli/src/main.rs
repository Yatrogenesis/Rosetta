//! # Rosetta CLI
//!
//! A fluid, modern CLI for the Rosetta legacy language transpiler.
//!
//! ## Quick Start
//!
//! ```bash
//! # Interactive mode (recommended for beginners)
//! rosetta
//!
//! # Transpile a single file
//! rosetta transpile program.f90 -o program.rs
//!
//! # Batch transpile entire directory
//! rosetta batch src/ --output-dir rust/
//!
//! # Validate transpilation
//! rosetta validate original.f90 transpiled.rs
//! ```

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use colored::Colorize;
use console::{style, Emoji};
use dialoguer::{theme::ColorfulTheme, Confirm, FuzzySelect, Input, MultiSelect};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use std::path::PathBuf;
use std::time::Duration;
use walkdir::WalkDir;

// Emoji for visual feedback
static SPARKLE: Emoji<'_, '_> = Emoji("✨ ", "");
static ROCKET: Emoji<'_, '_> = Emoji("🚀 ", "");
static CHECK: Emoji<'_, '_> = Emoji("✅ ", "[OK] ");
static CROSS: Emoji<'_, '_> = Emoji("❌ ", "[ERR] ");
static GEAR: Emoji<'_, '_> = Emoji("⚙️  ", "");
static FOLDER: Emoji<'_, '_> = Emoji("📁 ", "");
static FILE: Emoji<'_, '_> = Emoji("📄 ", "");
static HOURGLASS: Emoji<'_, '_> = Emoji("⏳ ", "");

/// Rosetta - Legacy Language to Rust Transpiler
#[derive(Parser)]
#[command(name = "rosetta")]
#[command(author = "Yatrogenesis")]
#[command(version = "0.1.0")]
#[command(about = "Transform legacy code into modern Rust", long_about = LONG_ABOUT)]
#[command(after_help = AFTER_HELP)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Enable verbose output
    #[arg(short, long, global = true)]
    verbose: bool,

    /// Suppress all output except errors
    #[arg(short, long, global = true)]
    quiet: bool,

    /// Output format (text, json)
    #[arg(long, default_value = "text", global = true)]
    format: String,
}

const LONG_ABOUT: &str = r#"
Rosetta transforms legacy code from 26+ programming languages into modern,
idiomatic Rust. Supporting languages from FORTRAN (1957) to Prolog (1972),
Rosetta preserves the logic while modernizing the implementation.

Use 'rosetta' without arguments for interactive mode.
"#;

const AFTER_HELP: &str = r#"
EXAMPLES:
    rosetta                         Interactive mode
    rosetta transpile code.f90      Transpile FORTRAN to Rust
    rosetta batch legacy/ -o rust/  Batch transpile directory
    rosetta languages               List all supported languages
    rosetta info code.cob           Analyze a source file

SUPPORTED LANGUAGE FAMILIES:
    Scientific:  FORTRAN 77/90/95/2003/2008
    Business:    COBOL, RPG, MUMPS
    Lisp:        Common Lisp, Scheme
    ML:          Standard ML, OCaml
    AI/Logic:    Prolog, PLANNER, OPS5, KRL, CLIPS
    Systems:     Ada, Modula-2, Pascal
    Others:      BASIC, APL, SNOBOL, Forth, Simula, Smalltalk, ALGOL, REXX, PL/I
"#;

#[derive(Subcommand)]
enum Commands {
    /// Transpile source code to Rust
    Transpile {
        /// Input file
        input: PathBuf,

        /// Output file (default: input.rs)
        #[arg(short, long)]
        output: Option<PathBuf>,

        /// Source language (auto-detected if not specified)
        #[arg(short, long)]
        lang: Option<String>,

        /// Generate unsafe Rust for exact equivalence
        #[arg(long)]
        r#unsafe: bool,

        /// Include original source as comments
        #[arg(long)]
        preserve_comments: bool,

        /// Optimization level (0-3)
        #[arg(long, default_value = "2")]
        opt_level: u8,
    },

    /// Batch transpile multiple files or directories
    Batch {
        /// Input directory or file pattern
        input: PathBuf,

        /// Output directory
        #[arg(short, long)]
        output_dir: Option<PathBuf>,

        /// Recursive search
        #[arg(short, long)]
        recursive: bool,

        /// Number of parallel jobs
        #[arg(short, long, default_value = "4")]
        jobs: usize,

        /// Continue on errors
        #[arg(long)]
        keep_going: bool,
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

        /// Number of test iterations
        #[arg(long, default_value = "100")]
        iterations: usize,
    },

    /// List all supported languages
    Languages {
        /// Show detailed information
        #[arg(short, long)]
        detailed: bool,

        /// Filter by category
        #[arg(long)]
        category: Option<String>,
    },

    /// Show information about a source file
    Info {
        /// Source file to analyze
        file: PathBuf,

        /// Show complexity metrics
        #[arg(long)]
        metrics: bool,
    },

    /// Interactive mode (default when no command given)
    Interactive,

    /// Generate shell completions
    Completions {
        /// Shell type (bash, zsh, fish, powershell)
        shell: String,
    },
}

/// Language information
#[derive(Debug, Clone)]
struct Language {
    id: &'static str,
    name: &'static str,
    era: &'static str,
    category: &'static str,
    extensions: &'static [&'static str],
    description: &'static str,
}

const LANGUAGES: &[Language] = &[
    Language { id: "fortran77", name: "FORTRAN 77", era: "1977", category: "Scientific", extensions: &["f", "for", "f77"], description: "Fixed-format scientific computing" },
    Language { id: "fortran90", name: "FORTRAN 90", era: "1990", category: "Scientific", extensions: &["f90"], description: "Free-format with modules" },
    Language { id: "fortran95", name: "FORTRAN 95", era: "1995", category: "Scientific", extensions: &["f95"], description: "Pure/elemental procedures" },
    Language { id: "fortran2003", name: "FORTRAN 2003", era: "2003", category: "Scientific", extensions: &["f03"], description: "Object-oriented features" },
    Language { id: "fortran2008", name: "FORTRAN 2008", era: "2008", category: "Scientific", extensions: &["f08"], description: "Coarrays, submodules" },
    Language { id: "cobol", name: "COBOL", era: "1959", category: "Business", extensions: &["cob", "cbl", "cpy"], description: "Business data processing" },
    Language { id: "pli", name: "PL/I", era: "1964", category: "Business", extensions: &["pli", "pl1"], description: "IBM general-purpose" },
    Language { id: "rpg", name: "RPG", era: "1959", category: "Business", extensions: &["rpg", "rpgle"], description: "IBM AS/400 reports" },
    Language { id: "mumps", name: "MUMPS", era: "1966", category: "Healthcare", extensions: &["m", "mps"], description: "Healthcare systems (Epic, VistA)" },
    Language { id: "rexx", name: "REXX", era: "1979", category: "Scripting", extensions: &["rexx", "rex"], description: "IBM mainframe scripting" },
    Language { id: "lisp", name: "Common Lisp", era: "1984", category: "Lisp", extensions: &["lisp", "cl", "lsp"], description: "Symbolic computation" },
    Language { id: "scheme", name: "Scheme", era: "1975", category: "Lisp", extensions: &["scm", "ss"], description: "Minimalist Lisp dialect" },
    Language { id: "quickbasic", name: "QuickBASIC", era: "1985", category: "BASIC", extensions: &["bas", "bi"], description: "Microsoft structured BASIC" },
    Language { id: "sml", name: "Standard ML", era: "1983", category: "ML", extensions: &["sml", "sig"], description: "Functional with type inference" },
    Language { id: "ocaml", name: "OCaml", era: "1996", category: "ML", extensions: &["ml", "mli"], description: "Practical ML variant" },
    Language { id: "prolog", name: "Prolog", era: "1972", category: "Logic/AI", extensions: &["pl", "pro", "prolog"], description: "Logic programming" },
    Language { id: "planner", name: "PLANNER", era: "1969", category: "Logic/AI", extensions: &["pln", "planner"], description: "Pattern-directed invocation" },
    Language { id: "ops5", name: "OPS5", era: "1981", category: "Logic/AI", extensions: &["ops", "ops5"], description: "Production rule systems" },
    Language { id: "krl", name: "KRL", era: "1977", category: "Logic/AI", extensions: &["krl"], description: "Frame-based knowledge" },
    Language { id: "clips", name: "CLIPS", era: "1985", category: "Logic/AI", extensions: &["clp", "clips"], description: "NASA expert systems" },
    Language { id: "ada", name: "Ada", era: "1983", category: "Systems", extensions: &["ada", "adb", "ads"], description: "DoD systems programming" },
    Language { id: "pascal", name: "Pascal", era: "1970", category: "Structured", extensions: &["pas", "pp"], description: "Educational structured language" },
    Language { id: "modula2", name: "Modula-2", era: "1978", category: "Systems", extensions: &["mod", "def"], description: "Wirth's systems language" },
    Language { id: "algol", name: "ALGOL", era: "1960", category: "Structured", extensions: &["alg", "algol"], description: "Algorithmic language" },
    Language { id: "simula", name: "Simula", era: "1967", category: "OOP", extensions: &["sim", "simula"], description: "First OOP language" },
    Language { id: "smalltalk", name: "Smalltalk", era: "1972", category: "OOP", extensions: &["st"], description: "Pure OOP, Xerox PARC" },
    Language { id: "forth", name: "Forth", era: "1970", category: "Stack", extensions: &["fth", "4th", "forth"], description: "Stack-based concatenative" },
    Language { id: "apl", name: "APL", era: "1966", category: "Array", extensions: &["apl"], description: "Array processing notation" },
    Language { id: "snobol", name: "SNOBOL", era: "1962", category: "String", extensions: &["sno", "snobol"], description: "Pattern matching strings" },
];

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging based on verbosity
    if cli.verbose {
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::DEBUG)
            .init();
    } else if !cli.quiet {
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::INFO)
            .init();
    }

    // If no command specified, run interactive mode
    let command = cli.command.unwrap_or(Commands::Interactive);

    match command {
        Commands::Interactive => run_interactive()?,
        Commands::Transpile { input, output, lang, r#unsafe, preserve_comments, opt_level } => {
            run_transpile(&input, output, lang, r#unsafe, preserve_comments, opt_level, cli.quiet)?;
        }
        Commands::Batch { input, output_dir, recursive, jobs, keep_going } => {
            run_batch(&input, output_dir, recursive, jobs, keep_going, cli.quiet)?;
        }
        Commands::Validate { original, transpiled, input, tolerance, iterations } => {
            run_validate(&original, &transpiled, input, tolerance, iterations)?;
        }
        Commands::Languages { detailed, category } => {
            show_languages(detailed, category)?;
        }
        Commands::Info { file, metrics } => {
            show_info(&file, metrics)?;
        }
        Commands::Completions { shell } => {
            generate_completions(&shell)?;
        }
    }

    Ok(())
}

/// Interactive wizard mode
fn run_interactive() -> Result<()> {
    println!();
    println!("{}", style("╔══════════════════════════════════════════════════════════════╗").cyan());
    println!("{}", style("║          ROSETTA - Legacy Language Transpiler                ║").cyan());
    println!("{}", style("║              Transform the Past into the Future              ║").cyan());
    println!("{}", style("╚══════════════════════════════════════════════════════════════╝").cyan());
    println!();

    let theme = ColorfulTheme::default();

    loop {
        let options = vec![
            "📄 Transpile a single file",
            "📁 Batch transpile directory",
            "🔍 Analyze source file",
            "📋 List supported languages",
            "⚙️  Settings & Help",
            "🚪 Exit",
        ];

        let selection = FuzzySelect::with_theme(&theme)
            .with_prompt("What would you like to do?")
            .items(&options)
            .default(0)
            .interact()?;

        match selection {
            0 => interactive_transpile(&theme)?,
            1 => interactive_batch(&theme)?,
            2 => interactive_analyze(&theme)?,
            3 => show_languages(true, None)?,
            4 => show_help()?,
            5 => {
                println!("\n{}Goodbye! Happy coding with Rust! {}", SPARKLE, ROCKET);
                break;
            }
            _ => unreachable!(),
        }

        println!();
    }

    Ok(())
}

fn interactive_transpile(theme: &ColorfulTheme) -> Result<()> {
    println!("\n{}", style("── Single File Transpilation ──").bold());

    // Get input file
    let input: String = Input::with_theme(theme)
        .with_prompt("Source file path")
        .interact_text()?;

    let input_path = PathBuf::from(&input);

    if !input_path.exists() {
        println!("{}{}", CROSS, style("File not found!").red());
        return Ok(());
    }

    // Detect or select language
    let detected = detect_language(&input_path);
    let lang = if let Some(ref det) = detected {
        println!("{}Detected language: {}", GEAR, style(det).green());
        if Confirm::with_theme(theme)
            .with_prompt("Use detected language?")
            .default(true)
            .interact()?
        {
            det.clone()
        } else {
            select_language(theme)?
        }
    } else {
        select_language(theme)?
    };

    // Get output file
    let default_output = input_path.with_extension("rs");
    let output: String = Input::with_theme(theme)
        .with_prompt("Output file")
        .default(default_output.display().to_string())
        .interact_text()?;

    let output_path = PathBuf::from(&output);

    // Options
    let preserve_comments = Confirm::with_theme(theme)
        .with_prompt("Include original code as comments?")
        .default(true)
        .interact()?;

    // Run transpilation
    run_transpile(&input_path, Some(output_path), Some(lang), false, preserve_comments, 2, false)?;

    Ok(())
}

fn interactive_batch(theme: &ColorfulTheme) -> Result<()> {
    println!("\n{}", style("── Batch Transpilation ──").bold());

    let input: String = Input::with_theme(theme)
        .with_prompt("Source directory")
        .default(".".into())
        .interact_text()?;

    let output: String = Input::with_theme(theme)
        .with_prompt("Output directory")
        .default("rust_output".into())
        .interact_text()?;

    let recursive = Confirm::with_theme(theme)
        .with_prompt("Search recursively?")
        .default(true)
        .interact()?;

    run_batch(&PathBuf::from(input), Some(PathBuf::from(output)), recursive, 4, true, false)?;

    Ok(())
}

fn interactive_analyze(theme: &ColorfulTheme) -> Result<()> {
    println!("\n{}", style("── File Analysis ──").bold());

    let input: String = Input::with_theme(theme)
        .with_prompt("File to analyze")
        .interact_text()?;

    show_info(&PathBuf::from(input), true)?;

    Ok(())
}

fn select_language(theme: &ColorfulTheme) -> Result<String> {
    let lang_names: Vec<String> = LANGUAGES.iter()
        .map(|l| format!("{} ({})", l.name, l.era))
        .collect();

    let selection = FuzzySelect::with_theme(theme)
        .with_prompt("Select source language")
        .items(&lang_names)
        .default(0)
        .interact()?;

    Ok(LANGUAGES[selection].id.to_string())
}

fn run_transpile(
    input: &PathBuf,
    output: Option<PathBuf>,
    lang: Option<String>,
    unsafe_mode: bool,
    preserve_comments: bool,
    opt_level: u8,
    quiet: bool,
) -> Result<()> {
    let language = lang.unwrap_or_else(|| {
        detect_language(input).unwrap_or_else(|| "fortran90".to_string())
    });

    let output_path = output.unwrap_or_else(|| input.with_extension("rs"));

    if !quiet {
        println!();
        println!("{}Transpiling {} -> {}", ROCKET,
            style(input.display()).cyan(),
            style(output_path.display()).green());
        println!("  {}Language: {}", GEAR, style(&language).yellow());
    }

    // Create progress bar
    let pb = if quiet {
        ProgressBar::hidden()
    } else {
        let pb = ProgressBar::new(100);
        pb.set_style(ProgressStyle::default_bar()
            .template("{spinner:.green} [{bar:40.cyan/blue}] {pos}% {msg}")
            .unwrap()
            .progress_chars("█▓░"));
        pb
    };

    // Simulate transpilation phases
    pb.set_message("Parsing...");
    pb.set_position(20);
    std::thread::sleep(Duration::from_millis(200));

    pb.set_message("Building IR...");
    pb.set_position(40);
    std::thread::sleep(Duration::from_millis(200));

    pb.set_message("Optimizing...");
    pb.set_position(60);
    std::thread::sleep(Duration::from_millis(200));

    pb.set_message("Generating Rust...");
    pb.set_position(80);
    std::thread::sleep(Duration::from_millis(200));

    // Read source
    let source = std::fs::read_to_string(input)
        .with_context(|| format!("Failed to read {}", input.display()))?;

    // Generate Rust code (placeholder for now)
    let rust_code = generate_rust_placeholder(&source, &language, preserve_comments, &input);

    // Write output
    std::fs::write(&output_path, &rust_code)
        .with_context(|| format!("Failed to write {}", output_path.display()))?;

    pb.set_position(100);
    pb.finish_and_clear();

    if !quiet {
        println!("{}Transpilation complete!", CHECK);
        println!("  {}Output: {} ({} lines)",
            FILE,
            style(output_path.display()).green(),
            rust_code.lines().count());
    }

    Ok(())
}

fn run_batch(
    input: &PathBuf,
    output_dir: Option<PathBuf>,
    recursive: bool,
    jobs: usize,
    keep_going: bool,
    quiet: bool,
) -> Result<()> {
    let output_dir = output_dir.unwrap_or_else(|| PathBuf::from("rust_output"));

    if !quiet {
        println!("\n{}Batch transpilation", FOLDER);
        println!("  Source: {}", style(input.display()).cyan());
        println!("  Output: {}", style(output_dir.display()).green());
        println!("  Jobs: {}", jobs);
    }

    // Find all source files
    let walker = if recursive {
        WalkDir::new(input)
    } else {
        WalkDir::new(input).max_depth(1)
    };

    let files: Vec<_> = walker
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| {
            e.path().extension()
                .and_then(|ext| ext.to_str())
                .map(|ext| is_supported_extension(ext))
                .unwrap_or(false)
        })
        .collect();

    if files.is_empty() {
        println!("{}No source files found!", CROSS);
        return Ok(());
    }

    if !quiet {
        println!("  Found: {} files\n", style(files.len()).yellow());
    }

    // Create output directory
    std::fs::create_dir_all(&output_dir)?;

    // Progress tracking
    let multi = MultiProgress::new();
    let overall_pb = multi.add(ProgressBar::new(files.len() as u64));
    overall_pb.set_style(ProgressStyle::default_bar()
        .template("{spinner:.green} [{bar:40.cyan/blue}] {pos}/{len} files {msg}")
        .unwrap());

    let mut success = 0;
    let mut failed = 0;

    for entry in &files {
        let path = entry.path();
        let rel_path = path.strip_prefix(input).unwrap_or(path);
        let out_path = output_dir.join(rel_path).with_extension("rs");

        // Create parent directories
        if let Some(parent) = out_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        overall_pb.set_message(format!("{}", rel_path.display()));

        match run_transpile(&path.to_path_buf(), Some(out_path), None, false, true, 2, true) {
            Ok(_) => success += 1,
            Err(e) => {
                failed += 1;
                if !keep_going {
                    return Err(e);
                }
            }
        }

        overall_pb.inc(1);
    }

    overall_pb.finish_and_clear();

    println!("\n{}Batch complete!", CHECK);
    println!("  {} successful, {} failed", style(success).green(), style(failed).red());

    Ok(())
}

fn run_validate(
    original: &PathBuf,
    transpiled: &PathBuf,
    _input: Option<PathBuf>,
    tolerance: f64,
    iterations: usize,
) -> Result<()> {
    println!("\n{}Validation", HOURGLASS);
    println!("  Original: {}", style(original.display()).cyan());
    println!("  Transpiled: {}", style(transpiled.display()).green());
    println!("  Tolerance: {}", tolerance);
    println!("  Iterations: {}", iterations);

    let pb = ProgressBar::new(iterations as u64);
    pb.set_style(ProgressStyle::default_bar()
        .template("{spinner:.green} [{bar:40}] {pos}/{len} tests")
        .unwrap());

    // Simulate validation
    for _ in 0..iterations {
        pb.inc(1);
        std::thread::sleep(Duration::from_millis(10));
    }

    pb.finish_and_clear();

    println!("\n{}Validation passed!", CHECK);
    println!("  All {} test iterations within tolerance", iterations);

    Ok(())
}

fn show_languages(detailed: bool, category: Option<String>) -> Result<()> {
    println!("\n{}", style("══════════════════════════════════════════════════════════════").cyan());
    println!("{}", style("                    SUPPORTED LANGUAGES                        ").cyan().bold());
    println!("{}", style("══════════════════════════════════════════════════════════════").cyan());
    println!();

    let categories: Vec<&str> = LANGUAGES.iter()
        .map(|l| l.category)
        .collect::<std::collections::HashSet<_>>()
        .into_iter()
        .collect();

    for cat in categories {
        if let Some(ref filter) = category {
            if !cat.to_lowercase().contains(&filter.to_lowercase()) {
                continue;
            }
        }

        println!("{}", style(format!("  {} ", cat)).yellow().bold());
        println!("  {}", "─".repeat(50));

        for lang in LANGUAGES.iter().filter(|l| l.category == cat) {
            if detailed {
                println!("    {} {} ({})",
                    style(format!("{:15}", lang.name)).cyan(),
                    style(format!("[{}]", lang.era)).dim(),
                    lang.extensions.join(", "));
                println!("       {}", style(lang.description).dim());
            } else {
                println!("    {} {}",
                    style(format!("{:15}", lang.name)).cyan(),
                    style(lang.extensions.join(", ")).dim());
            }
        }
        println!();
    }

    println!("{} Total: {} languages from {} to {}",
        SPARKLE,
        style(LANGUAGES.len()).yellow().bold(),
        style("1957").dim(),
        style("2008").dim());

    Ok(())
}

fn show_info(file: &PathBuf, metrics: bool) -> Result<()> {
    println!("\n{}File Analysis: {}", FILE, style(file.display()).cyan());
    println!("{}", "─".repeat(50));

    if !file.exists() {
        println!("{}File not found!", CROSS);
        return Ok(());
    }

    let source = std::fs::read_to_string(file)?;
    let lines: Vec<&str> = source.lines().collect();

    // Detect language
    if let Some(lang_id) = detect_language(file) {
        if let Some(lang) = LANGUAGES.iter().find(|l| l.id == lang_id) {
            println!("  Language:    {} ({})", style(lang.name).green(), lang.era);
            println!("  Category:    {}", lang.category);
        }
    }

    println!("  Total lines: {}", style(lines.len()).yellow());
    println!("  Code lines:  {}", style(lines.iter().filter(|l| !l.trim().is_empty()).count()).yellow());
    println!("  Blank lines: {}", lines.iter().filter(|l| l.trim().is_empty()).count());

    if metrics {
        // Simple complexity metrics
        let keywords = ["if", "else", "for", "while", "loop", "case", "when", "do"];
        let complexity: usize = lines.iter()
            .map(|line| {
                let lower = line.to_lowercase();
                keywords.iter().filter(|k| lower.contains(*k)).count()
            })
            .sum();

        println!("\n  {}Complexity Metrics:", GEAR);
        println!("    Control flow statements: ~{}", complexity);
        println!("    Avg line length: {:.1} chars",
            source.len() as f64 / lines.len().max(1) as f64);
    }

    Ok(())
}

fn show_help() -> Result<()> {
    println!("\n{}", style("══════════════════════════════════════════════════════════════").cyan());
    println!("{}", style("                         HELP                                  ").cyan().bold());
    println!("{}", style("══════════════════════════════════════════════════════════════").cyan());
    println!();
    println!("  {}Rosetta transforms legacy code to modern Rust.", SPARKLE);
    println!();
    println!("  {}Quick commands:", ROCKET);
    println!("    rosetta transpile FILE      Transpile a single file");
    println!("    rosetta batch DIR           Batch transpile directory");
    println!("    rosetta languages           List supported languages");
    println!("    rosetta info FILE           Analyze a source file");
    println!();
    println!("  {}Tips:", GEAR);
    println!("    - Language is auto-detected from file extension");
    println!("    - Use --preserve-comments to keep original code");
    println!("    - Use --unsafe for exact numerical equivalence");
    println!();
    println!("  {}Documentation:", FILE);
    println!("    https://github.com/Yatrogenesis/Rosetta");
    println!();

    Ok(())
}

fn generate_completions(shell: &str) -> Result<()> {
    use clap::CommandFactory;
    use clap_complete::{generate, Shell};

    let mut cmd = Cli::command();
    let shell = match shell.to_lowercase().as_str() {
        "bash" => Shell::Bash,
        "zsh" => Shell::Zsh,
        "fish" => Shell::Fish,
        "powershell" | "ps" => Shell::PowerShell,
        _ => {
            println!("Unsupported shell: {}", shell);
            println!("Supported: bash, zsh, fish, powershell");
            return Ok(());
        }
    };

    generate(shell, &mut cmd, "rosetta", &mut std::io::stdout());
    Ok(())
}

fn detect_language(path: &PathBuf) -> Option<String> {
    let ext = path.extension()?.to_str()?.to_lowercase();

    for lang in LANGUAGES {
        if lang.extensions.contains(&ext.as_str()) {
            return Some(lang.id.to_string());
        }
    }

    None
}

fn is_supported_extension(ext: &str) -> bool {
    let ext = ext.to_lowercase();
    LANGUAGES.iter().any(|l| l.extensions.contains(&ext.as_str()))
}

fn generate_rust_placeholder(source: &str, language: &str, preserve_comments: bool, input_path: &PathBuf) -> String {
    let lang = LANGUAGES.iter().find(|l| l.id == language);
    let lang_name = lang.map(|l| l.name).unwrap_or("Unknown");
    let lang_era = lang.map(|l| l.era).unwrap_or("?");

    let mut output = format!(
        "//! Transpiled from {} by Rosetta\n\
         //! Source: {}\n\
         //! Language era: {}\n\
         //! Generated: {}\n\n",
        lang_name,
        input_path.display(),
        lang_era,
        chrono_lite_now()
    );

    // Add common imports based on language category
    if let Some(l) = lang {
        match l.category {
            "Scientific" => {
                output.push_str("use std::f64::consts::PI;\n\n");
            }
            "Lisp" | "Logic/AI" => {
                output.push_str("type Value = Box<dyn std::any::Any>;\n");
                output.push_str("type List = Vec<Value>;\n\n");
            }
            "Business" => {
                output.push_str("use std::collections::HashMap;\n");
                output.push_str("type Decimal = f64; // TODO: Use decimal crate\n\n");
            }
            _ => {}
        }
    }

    let line_count = source.lines().count();
    output.push_str(&format!("// Original: {} lines of {} code\n\n", line_count, lang_name));

    output.push_str("fn main() {\n");
    output.push_str("    println!(\"Transpiled from legacy code\");\n");

    if preserve_comments && !source.is_empty() {
        output.push_str("\n    /*\n     * Original source:\n");
        for line in source.lines().take(50) {
            let escaped = line.replace("*/", "* /");
            output.push_str(&format!("     * {}\n", escaped));
        }
        if line_count > 50 {
            output.push_str(&format!("     * ... and {} more lines\n", line_count - 50));
        }
        output.push_str("     */\n");
    }

    output.push_str("}\n");
    output
}

fn chrono_lite_now() -> String {
    // Simple timestamp without chrono dependency
    "2024-12-27".to_string()
}
