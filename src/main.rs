use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;

use acc::cfg::def::ControlFlowGraph;
use acc::cfg::lower::{LowerError, lower_ast_to_cfg};
use acc::cfg::{OptPass, OptPassConfig, sema};
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

    /// Optimization passes to run, in order.
    #[clap(long, value_enum, value_name = "OPT", value_delimiter = ',', num_args = 1.., default_value = "full")]
    opt: Vec<OptMetaStageRef>,

    /// Insert debug dumps before and after each mentioned optimization pass in the optimization pipeline.
    #[clap(long, value_enum, value_name = "OPT", value_delimiter = ',', num_args = 0..)]
    opt_debug: Vec<OptStageRef>,

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

#[derive(Debug, clap::ValueEnum, Clone, Copy)]
enum OptStageRef {
    /// Constant Propagation
    #[value(name = "cp")]
    ConstantPropagation,
    /// Dead Value Elimination
    #[value(name = "dve")]
    DeadValueElimination,
    /// Constant Propagation + Dead Value Elimination + Trim Dead Blocks Loop
    #[value(name = "cpdvetdb")]
    ConstantPropagationDeadValueEliminationTrimDeadBlocksLoop,
    /// Block Inliner
    #[value(name = "bi")]
    BlockInliner,
    /// Hoist Pass
    #[value(name = "hp")]
    HoistPass,
    /// Phi Simplification
    #[value(name = "ps")]
    PhiSimplification,
    /// Value Inliner
    #[value(name = "vi")]
    ValueInliner,
    /// Value Inliner + Phi Simplification Loop
    #[value(name = "vips")]
    ValueInlinerPhiSimplificationLoop,
    /// Phi to Select pass
    #[value(name = "phi2sel")]
    PhiToSelect,
    /// Block Deduplication
    #[value(name = "bd")]
    BlockDedup,
    /// Tail Unification
    #[value(name = "tu")]
    TailUnification,
    /// Trim Dead Blocks
    #[value(name = "tdb")]
    TrimDeadBlocks,
}

impl From<OptStageRef> for OptPass {
    fn from(stage: OptStageRef) -> Self {
        match stage {
            OptStageRef::ConstantPropagation => OptPass::ConstantPropagation,
            OptStageRef::DeadValueElimination => OptPass::DeadValueElimination,
            OptStageRef::ConstantPropagationDeadValueEliminationTrimDeadBlocksLoop => {
                OptPass::CpDveAndTdbLoop
            }
            OptStageRef::BlockInliner => OptPass::BlockInliner,
            OptStageRef::HoistPass => OptPass::HoistPass,
            OptStageRef::PhiSimplification => OptPass::PhiSimplification,
            OptStageRef::ValueInliner => OptPass::ValueInliner,
            OptStageRef::ValueInlinerPhiSimplificationLoop => {
                OptPass::ValueInlinerPhiSimplificationLoop
            }
            OptStageRef::PhiToSelect => OptPass::PhiToSelect,
            OptStageRef::BlockDedup => OptPass::BlockDedup,
            OptStageRef::TailUnification => OptPass::TailUnification,
            OptStageRef::TrimDeadBlocks => OptPass::TrimDeadBlocks,
        }
    }
}

#[derive(Debug, clap::ValueEnum, Clone, Copy)]
enum OptMetaStageRef {
    #[value(name = "none")]
    None,
    /// Full pipeline of optimizations [full = vips,cpdvetdb,bi,hp,vips,phi2sel,bd,tu,dve,tdb,bi]
    #[value(name = "full")]
    Full,
    /// Constant Propagation
    #[value(name = "cp")]
    ConstantPropagation,
    /// Dead Value Elimination
    #[value(name = "dve")]
    DeadValueElimination,
    /// Constant Propagation + Dead Value Elimination + Trim Dead Blocks Loop
    #[value(name = "cpdvetdb")]
    ConstantPropagationDeadValueEliminationTrimDeadBlocksLoop,
    /// Block Inliner
    #[value(name = "bi")]
    BlockInliner,
    /// Hoist Pass
    #[value(name = "hp")]
    HoistPass,
    /// Phi Simplification
    #[value(name = "ps")]
    PhiSimplification,
    /// Value Inliner
    #[value(name = "vi")]
    ValueInliner,
    /// Value Inliner + Phi Simplification Loop
    #[value(name = "vips")]
    ValueInlinerPhiSimplificationLoop,
    /// Phi to Select pass
    #[value(name = "phi2sel")]
    PhiToSelect,
    /// Block Deduplication
    #[value(name = "bd")]
    BlockDedup,
    /// Tail Unification
    #[value(name = "tu")]
    TailUnification,
    /// Trim Dead Blocks
    #[value(name = "tdb")]
    TrimDeadBlocks,
    /// Debug dump before and after the next pass
    #[value(name = "dbg")]
    Debug,
    /// Debug Graphviz dump before and after the next pass
    #[value(name = "dbgv")]
    DebugGraphviz,
}

impl OptMetaStageRef {
    fn build_pipeline(passes: &[OptMetaStageRef], debug: &[OptStageRef]) -> OptPassConfig {
        let mut opt_config = OptPassConfig::new(vec![]);
        let mut wrap_with_debug = false;
        let mut wrap_with_debug_graphviz = false;

        macro_rules! wrap {
            ($name:literal, $stmt:stmt) => {
                if wrap_with_debug {
                    opt_config.push_pass(OptPass::Debug {
                        name: concat!("before ", $name),
                    });
                }
                if wrap_with_debug_graphviz {
                    opt_config.push_pass(OptPass::DebugGraphviz {
                        name: concat!("before ", $name),
                    });
                }
                $stmt
                if wrap_with_debug {
                    opt_config.push_pass(OptPass::Debug { name: concat!("after ", $name) });
                }
                if wrap_with_debug_graphviz {
                    opt_config.push_pass(OptPass::DebugGraphviz { name: concat!("after ", $name) });
                }
                wrap_with_debug = false;
                wrap_with_debug_graphviz = false;
            };
        }

        for pass in passes {
            match pass {
                OptMetaStageRef::None => {}
                OptMetaStageRef::Full => {
                    wrap!("Full", opt_config.join(OptPassConfig::full()));
                }
                OptMetaStageRef::ConstantPropagation => {
                    wrap!(
                        "ConstantPropagation",
                        opt_config.push_pass(OptPass::ConstantPropagation)
                    );
                }
                OptMetaStageRef::DeadValueElimination => {
                    wrap!(
                        "DeadValueElimination",
                        opt_config.push_pass(OptPass::DeadValueElimination)
                    );
                }
                OptMetaStageRef::ConstantPropagationDeadValueEliminationTrimDeadBlocksLoop => {
                    wrap!(
                        "ConstantPropagationDeadValueEliminationTrimDeadBlocksLoop",
                        opt_config.push_pass(OptPass::CpDveAndTdbLoop)
                    );
                }
                OptMetaStageRef::BlockInliner => {
                    wrap!("BlockInliner", opt_config.push_pass(OptPass::BlockInliner));
                }
                OptMetaStageRef::HoistPass => {
                    wrap!("HoistPass", opt_config.push_pass(OptPass::HoistPass));
                }
                OptMetaStageRef::PhiSimplification => {
                    wrap!(
                        "PhiSimplification",
                        opt_config.push_pass(OptPass::PhiSimplification)
                    );
                }
                OptMetaStageRef::ValueInliner => {
                    wrap!("ValueInliner", opt_config.push_pass(OptPass::ValueInliner));
                }
                OptMetaStageRef::ValueInlinerPhiSimplificationLoop => {
                    wrap!(
                        "ValueInlinerPhiSimplificationLoop",
                        opt_config.push_pass(OptPass::ValueInlinerPhiSimplificationLoop)
                    );
                }
                OptMetaStageRef::PhiToSelect => {
                    wrap!("PhiToSelect", opt_config.push_pass(OptPass::PhiToSelect));
                }
                OptMetaStageRef::BlockDedup => {
                    wrap!("BlockDedup", opt_config.push_pass(OptPass::BlockDedup));
                }
                OptMetaStageRef::TailUnification => {
                    wrap!(
                        "TailUnification",
                        opt_config.push_pass(OptPass::TailUnification)
                    );
                }
                OptMetaStageRef::TrimDeadBlocks => {
                    wrap!(
                        "TrimDeadBlocks",
                        opt_config.push_pass(OptPass::TrimDeadBlocks)
                    );
                }
                OptMetaStageRef::Debug => {
                    wrap_with_debug = true;
                }
                OptMetaStageRef::DebugGraphviz => {
                    wrap_with_debug_graphviz = true;
                }
            }
        }

        opt_config
            .insert_debug_passes(&debug.iter().copied().map(OptPass::from).collect::<Vec<_>>());

        opt_config
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
    let opt_config = OptMetaStageRef::build_pipeline(&args.opt, &args.opt_debug);

    for top_level in &program.items {
        match &**top_level {
            TopLevel::Function(func, var_id) => {
                let cfg = match lower_ast_to_cfg(func, &opt_config, &sema, &mut warnings) {
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
                TopLevel::Function(_, var_id) => {
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
