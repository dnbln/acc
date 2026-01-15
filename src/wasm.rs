use std::fmt::Write as _;

use serde::Serialize;
use wasm_bindgen::prelude::*;

use crate::cfg::{
    OptPass, OptPassConfig, display,
    lower::{CfgWarning, LowerError, lower_ast_to_cfg},
    sema,
};
use crate::parser::ast::Program;
use crate::parser::error::{ParseError, ParseErrorKind};
use crate::parser::span::Span;
use crate::parser::{Parser as CParser, TopLevel};

const GRAPHVIZ_HEADER: &str = r#"  node [shape=box, fontname="Courier New", fontsize=10];
  edge [fontname="Courier New", fontsize=9];
"#;

#[derive(Serialize)]
struct Range {
    start: usize,
    end: usize,
}

#[derive(Serialize)]
struct Diagnostic {
    range: Option<Range>,
    error_name: String,
    severity: String,
}

#[derive(Serialize)]
struct CompilationReport {
    ok: bool,
    ast: Option<Program>,
    cfg_text: Option<String>,
    graphviz: Option<String>,
    errors: Vec<Diagnostic>,
    warnings: Vec<Diagnostic>,
}

fn span_to_range(span: Span) -> Range {
    Range {
        start: span.start,
        end: span.end,
    }
}

fn parse_error_name(kind: &ParseErrorKind) -> &'static str {
    match kind {
        ParseErrorKind::UnexpectedToken { .. } => "UnexpectedToken",
        ParseErrorKind::UnexpectedEof { .. } => "UnexpectedEof",
        ParseErrorKind::InvalidToken => "InvalidToken",
        ParseErrorKind::UnknownPrefixOperator(_) => "UnknownPrefixOperator",
        ParseErrorKind::ExpectedExpression { .. } => "ExpectedExpression",
        ParseErrorKind::ExpectedType { .. } => "ExpectedType",
        ParseErrorKind::ExpectedIdentifier { .. } => "ExpectedIdentifier",
        ParseErrorKind::UnclosedDelimiter { .. } => "UnclosedDelimiter",
    }
}

fn parse_error_to_diag(err: &ParseError) -> Diagnostic {
    Diagnostic {
        range: Some(span_to_range(err.span)),
        error_name: parse_error_name(&err.kind).to_string(),
        severity: "error".to_string(),
    }
}

fn sema_error_to_diag(err: &sema::SemaError) -> Diagnostic {
    match err {
        sema::SemaError::UndefinedVariable(name_span, _ref_id) => Diagnostic {
            range: Some(span_to_range(name_span.span)),
            error_name: "UndefinedVariable".to_string(),
            severity: "error".to_string(),
        },
        sema::SemaError::RedeclaredVariable { new_span, .. } => Diagnostic {
            range: Some(span_to_range(new_span.span)),
            error_name: "RedeclaredVariable".to_string(),
            severity: "error".to_string(),
        },
    }
}

fn cfg_warning_to_diag(warning: &CfgWarning) -> Diagnostic {
    match warning {
        CfgWarning::UnreachableCode(span) => Diagnostic {
            range: Some(span_to_range(*span)),
            error_name: "UnreachableCode".to_string(),
            severity: "warning".to_string(),
        },
        CfgWarning::UnusedValue(span) => Diagnostic {
            range: Some(span_to_range(*span)),
            error_name: "UnusedValue".to_string(),
            severity: "warning".to_string(),
        },
    }
}

fn parse_opt_pass(name: &str) -> Result<OptPass, String> {
    match name {
        "cp" => Ok(OptPass::ConstantPropagation),
        "dve" => Ok(OptPass::DeadValueElimination),
        "cpdvetdb" => Ok(OptPass::CpDveAndTdbLoop),
        "bi" => Ok(OptPass::BlockInliner),
        "hp" => Ok(OptPass::HoistPass),
        "ps" => Ok(OptPass::PhiSimplification),
        "vi" => Ok(OptPass::ValueInliner),
        "vips" => Ok(OptPass::ValueInlinerPhiSimplificationLoop),
        "phi2sel" => Ok(OptPass::PhiToSelect),
        "bd" => Ok(OptPass::BlockDedup),
        "tu" => Ok(OptPass::TailUnification),
        "tdb" => Ok(OptPass::TrimDeadBlocks),
        _ => Err(format!("unknown opt pass '{}'", name)),
    }
}

fn build_opt_config(opts: &[String], dve_emit_warnings: bool) -> Result<OptPassConfig, String> {
    if opts.is_empty() {
        return Ok(OptPassConfig::full());
    }

    let mut passes = Vec::new();
    let mut has_full = false;
    let mut has_none = false;

    for opt in opts {
        match opt.as_str() {
            "full" => has_full = true,
            "none" => has_none = true,
            other => passes.push(parse_opt_pass(other)?),
        }
    }

    match (has_full, has_none, passes.is_empty()) {
        (true, false, true) => Ok(OptPassConfig::full()),
        (false, true, true) => Ok(OptPassConfig::new(Vec::new(), dve_emit_warnings)),
        (false, false, _) => Ok(OptPassConfig::new(passes, dve_emit_warnings)),
        _ => Err("use either 'full', 'none', or an explicit list of passes".to_string()),
    }
}

fn lower_cfgs(
    program: &Program,
    sema: &sema::SemaResults,
    opt_config: &OptPassConfig,
) -> Result<
    (
        Vec<(String, crate::cfg::def::ControlFlowGraph)>,
        Vec<CfgWarning>,
    ),
    LowerError,
> {
    let mut warnings = Vec::new();
    let mut cfgs = Vec::new();

    for top_level in &program.items {
        if let TopLevel::Function(func, _) = &**top_level {
            let cfg = lower_ast_to_cfg(func, opt_config, sema, &mut warnings)?;
            cfgs.push((func.name.node.clone(), cfg));
        }
    }

    Ok((cfgs, warnings))
}

fn sanitize_graphviz_name(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() { "_".to_string() } else { out }
}

#[wasm_bindgen]
pub fn compile(
    source: &str,
    opts: Vec<String>,
    dve_emit_warnings: bool,
) -> Result<JsValue, JsValue> {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    let program = match CParser::new(source) {
        Ok(mut parser) => match parser.parse_program() {
            Ok(program) => program,
            Err(error) => {
                errors.push(parse_error_to_diag(&error));
                return report_to_js(CompilationReport {
                    ok: false,
                    ast: None,
                    cfg_text: None,
                    graphviz: None,
                    errors,
                    warnings,
                });
            }
        },
        Err(parse_errors) => {
            errors.extend(parse_errors.iter().map(parse_error_to_diag));
            return report_to_js(CompilationReport {
                ok: false,
                ast: None,
                cfg_text: None,
                graphviz: None,
                errors,
                warnings,
            });
        }
    };

    let sema = match sema::sema(&program) {
        Ok(sema) => sema,
        Err(sema_errors) => {
            errors.extend(sema_errors.iter().map(sema_error_to_diag));
            return report_to_js(CompilationReport {
                ok: false,
                ast: None,
                cfg_text: None,
                graphviz: None,
                errors,
                warnings,
            });
        }
    };

    let opt_config =
        build_opt_config(&opts, dve_emit_warnings).map_err(|err| JsValue::from_str(&err))?;
    let (cfgs, cfg_warnings) = match lower_cfgs(&program, &sema, &opt_config) {
        Ok(result) => result,
        Err(lower_error) => {
            let mut gviz = None;
            match lower_error {
                LowerError::NoBody => errors.push(Diagnostic {
                    range: None,
                    error_name: "NoBody".to_string(),
                    severity: "error".to_string(),
                }),
                LowerError::SemanticErrors(items) => {
                    for item in items {
                        let name = match item {
                            crate::cfg::lower::SemanticError::ContinueOutsideLoop => {
                                "ContinueOutsideLoop"
                            }
                            crate::cfg::lower::SemanticError::BreakOutsideLoop => {
                                "BreakOutsideLoop"
                            }
                        };
                        errors.push(Diagnostic {
                            range: None,
                            error_name: name.to_string(),
                            severity: "error".to_string(),
                        });
                    }
                }
                LowerError::CfgValidationErrors(items) => {
                    for item in items {
                        errors.push(Diagnostic {
                            range: None,
                            error_name: format!("CfgValidation::{:?}", item),
                            severity: "error".to_string(),
                        });
                    }
                }
                LowerError::MalformedPhiInfo(info) => {
                    gviz = Some(info.display_in_graphviz(&sema));
                    errors.push(Diagnostic {
                        range: None,
                        error_name: "MalformedPhiInfo".to_string(),
                        severity: "error".to_string(),
                    });
                }
            }

            return report_to_js(CompilationReport {
                ok: false,
                ast: None,
                cfg_text: None,
                graphviz: gviz,
                errors,
                warnings,
            });
        }
    };

    warnings.extend(cfg_warnings.iter().map(cfg_warning_to_diag));
    let ast = program.clone();
    let mut cfg_text = String::new();
    for (name, cfg) in &cfgs {
        let _ = writeln!(
            cfg_text,
            "CFG for function {}:\n{}",
            name,
            cfg.display_with_sema(&sema)
        );
    }

    let mut graphviz = String::new();
    graphviz.push_str("digraph CFGs {\n");
    graphviz.push_str(GRAPHVIZ_HEADER);
    for (name, cfg) in cfgs {
        let safe_name = sanitize_graphviz_name(&name);
        graphviz.push_str(&display::make_graphviz_subgraph(&safe_name, &cfg, &sema));
    }
    graphviz.push_str("}\n");

    report_to_js(CompilationReport {
        ok: true,
        ast: Some(ast),
        cfg_text: Some(cfg_text),
        graphviz: Some(graphviz),
        errors,
        warnings,
    })
}

fn report_to_js(report: CompilationReport) -> Result<JsValue, JsValue> {
    serde_wasm_bindgen::to_value(&report).map_err(|err| JsValue::from_str(&err.to_string()))
}
