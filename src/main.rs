use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;

use acc::cfg::def::ControlFlowGraph;
use acc::cfg::lower::LowerError;
use acc::cfg::sema;
use acc::diagnostics::{IntoDiagnostic, show_diagnostics, show_diagnostics_with_sema};
use acc::parser::ast::{RefId, VarId};
use acc::parser::{Parser as CParser, TopLevel};
use anyhow::{Result, anyhow, bail};
use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(value_name = "FILE")]
    path: PathBuf,
    /// Display the AST after parsing
    #[clap(long)]
    ast: bool,
    /// Display resolved names after semantic analysis
    #[clap(long)]
    resolve: bool,

    /// Display the CFGs after lowering
    #[clap(long)]
    cfg: bool,

    /// Generate Graphviz DOT files for CFGs after lowering, under directory DIR
    #[clap(long, value_name = "DIR")]
    cfg_graphviz: Option<PathBuf>,

    /// Display the generated LLVM IR
    #[cfg(feature = "llvm-backend")]
    #[clap(long)]
    llvm_ir: bool,

    /// Display the optimized LLVM IR
    #[cfg(feature = "llvm-backend")]
    #[clap(long)]
    llvm_optimized_ir: bool,

    /// Output the generated LLVM IR to the specified file
    #[cfg(feature = "llvm-backend")]
    #[clap(short = 'o', value_name = "FILE")]
    llvm_output: Option<PathBuf>,
}

pub struct SemaTargetDisplay {
    ref_id: RefId,
    ref_span: acc::parser::span::Span,
    var_id: VarId,
    var_span: acc::parser::span::Span,
}

impl IntoDiagnostic for SemaTargetDisplay {
    fn to_diagnostic(&self, file_id: usize) -> codespan_reporting::diagnostic::Diagnostic<usize> {
        codespan_reporting::diagnostic::Diagnostic::note()
            .with_message("Name resolution")
            .with_labels(vec![
                codespan_reporting::diagnostic::Label::primary(file_id, self.ref_span.range())
                    .with_message(format!("Ref at {:?}", self.ref_id)),
                codespan_reporting::diagnostic::Label::secondary(file_id, self.var_span.range())
                    .with_message(format!("Resolved to {:?}", self.var_id)),
            ])
    }
}

fn display_cfg_errors(
    source: impl AsRef<str>,
    filename: impl AsRef<str>,
    errors: &LowerError,
    sema: &sema::SemaResults,
) {
    match errors {
        LowerError::MalformedPhiInfo(infos) => {
            show_diagnostics_with_sema(source, filename, &infos.malformed_assignments, sema);

            eprintln!("---\n{}", infos.display_in_graphviz(sema));
        }
        _ => {}
    }
}

fn main() -> Result<()> {
    let args = Args::parse();
    let path = args.path;
    let source = fs::read_to_string(&path).unwrap();

    let program = match CParser::new(&source) {
        Ok(mut parser) => match parser.parse_program() {
            Ok(program) => program,
            Err(e) => {
                show_diagnostics(&source, path.to_string_lossy().as_ref(), &[e]);
                return Err(anyhow!("Parsing failed"));
            }
        },
        Err(errors) => {
            show_diagnostics(&source, path.to_string_lossy(), &errors);
            bail!("Could not tokenize input");
        }
    };

    if args.ast {
        for stmt in &program.items {
            match &**stmt {
                TopLevel::Function(func, _) => {
                    println!("Parsed function: {}", *func.name);
                    println!("Parameters: {:#?}", func.params);
                    println!("Return Type: {:#?}", func.return_type);
                    println!("Body: {:#?}", func.body);
                }
                _ => {
                    println!("Parsed top-level item.");
                }
            }
        }
    }

    let resolved = sema::sema(&program);
    let sema = match resolved {
        Ok(r) => r,
        Err(errors) => {
            show_diagnostics(&source, path.to_string_lossy(), &errors);
            bail!("Semantic analysis failed");
        }
    };

    if args.resolve {
        let mut displays = Vec::new();
        for (ref_id, var_id) in &sema.m {
            let ref_span = sema.refs.get(ref_id).unwrap().clone();
            let var_span = sema.vars.get(var_id).unwrap().clone();
            displays.push(SemaTargetDisplay {
                ref_id: *ref_id,
                ref_span,
                var_id: *var_id,
                var_span,
            });
        }

        show_diagnostics(&source, path.to_string_lossy(), &displays);
    }

    let mut warnings = Vec::new();

    let mut program_cfgs = BTreeMap::<VarId, ControlFlowGraph>::new();

    for top_level in &program.items {
        match &**top_level {
            TopLevel::Function(func, var_id) => {
                let cfg = match acc::cfg::lower::lower_ast_to_cfg(func, &sema, &mut warnings) {
                    Ok(cfg) => cfg,
                    Err(error) => {
                        eprintln!("CFG lowering error in function {}: {:?}", *func.name, error);

                        display_cfg_errors(&source, path.to_string_lossy(), &error, &sema);

                        bail!("CFG lowering failed");
                    }
                };

                if args.cfg {
                    println!(
                        "CFG for function {}:\n{}",
                        *func.name,
                        cfg.display_with_sema(&sema)
                    );
                }

                if let Some(cfg_graphviz_dir) = &args.cfg_graphviz {
                    if !cfg_graphviz_dir.exists() {
                        fs::create_dir_all(cfg_graphviz_dir).map_err(|e| {
                            anyhow!(
                                "Failed to create directory {}: {}",
                                cfg_graphviz_dir.display(),
                                e
                            )
                        })?;
                    }

                    let dot_output = acc::cfg::display::graphviz(&cfg, &sema);
                    let dot_filename = cfg_graphviz_dir.join(format!("{}.dot", func.name.node));
                    fs::write(&dot_filename, dot_output).map_err(|e| {
                        anyhow!(
                            "Failed to write Graphviz DOT file {}: {}",
                            dot_filename.display(),
                            e
                        )
                    })?;
                    println!("Wrote CFG Graphviz DOT file to {}", dot_filename.display());
                }

                program_cfgs.insert(*var_id, cfg);
            }
            _ => {}
        }
    }

    if !warnings.is_empty() {
        show_diagnostics(&source, path.to_string_lossy(), &warnings);
    }

    #[cfg(feature = "llvm-backend")]
    {
        let mut backend = acc::backend::Backend::new(sema);

        backend.init_functions(&program);

        for top_level in &program.items {
            match &**top_level {
                TopLevel::Function(func, var_id) => {
                    let cfg = &program_cfgs[var_id];
                    backend.lower_function(*var_id, &cfg);
                }
                _ => {}
            }
        }

        if args.llvm_ir {
            println!("Generated LLVM IR:");
            backend.debug();
        }
        backend.optimize();
        if args.llvm_optimized_ir {
            println!("Optimized LLVM IR:");
            backend.debug();
        }

        if let Some(p) = args.llvm_output {
            backend.write_bitcode_to_file(&p)?;
        }
    }

    Ok(())
}
