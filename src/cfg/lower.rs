use std::collections::{BTreeMap, BTreeSet};

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    cfg::{
        builder::{CfgBuilder, ValidateError},
        def::*,
        sema::SemaResults,
    },
    diagnostics::{IntoDiagnostic, SemanticsAwareIntoDiagnostic},
    parser::{
        Expr, Function, Stmt,
        ast::VarId,
        operator::{BinaryOp, UnaryOp},
        span::{Span, Spanned},
    },
};

#[derive(Debug)]
pub enum SemanticError {
    ContinueOutsideLoop,
    BreakOutsideLoop,
}

#[derive(Debug)]
pub enum CfgWarning {
    UnreachableCode(Span),
}

impl IntoDiagnostic for CfgWarning {
    fn to_diagnostic(&self, file_id: usize) -> codespan_reporting::diagnostic::Diagnostic<usize> {
        match self {
            CfgWarning::UnreachableCode(span) => {
                codespan_reporting::diagnostic::Diagnostic::warning()
                    .with_message("unreachable code")
                    .with_labels(vec![
                        codespan_reporting::diagnostic::Label::primary(file_id, span.range())
                            .with_message("this code is unreachable"),
                    ])
            }
        }
    }
}

#[derive(Debug)]
pub enum LowerError {
    NoBody,
    SemanticErrors(Vec<SemanticError>),
    CfgValidationErrors(Vec<ValidateError>),
    MalformedPhiInfo(MalformedPhiInfo),
}

pub fn lower_ast_to_cfg(
    program: &Vec<Spanned<crate::parser::TopLevel>>,
    ast: &Function,
    sema: &SemaResults,
    warnings: &mut Vec<CfgWarning>,
) -> Result<ControlFlowGraph, LowerError> {
    let mut builder = CfgBuilder::new();

    let Some(body) = &ast.body else {
        return Err(LowerError::NoBody);
    };

    let entry = builder.create_bb();
    let mut bb = entry;

    for def in program {
        match &**def {
            crate::parser::TopLevel::GlobalVar { .. } => {}
            crate::parser::TopLevel::Function(f, var) => {
                let v = builder.allocate_value(def.span, Some(*var));
                builder.add_instruction(
                    entry,
                    CfgInstruction::Assign {
                        dest: v,
                        val: RValue::Function {
                            name: f.name.node.clone(),
                        },
                    },
                );
                builder.add_instruction(
                    entry,
                    CfgInstruction::_AssignVar {
                        var_id: *var,
                        val: RValue::Value(v),
                    },
                );
            }
        }
    }

    for (param_index, (_, _, var_id)) in ast.params.iter().enumerate() {
        let v = builder.allocate_value(sema.var_span(*var_id), Some(*var_id));
        builder.add_instruction(
            entry,
            CfgInstruction::Assign {
                dest: v,
                val: RValue::Param { param_index },
            },
        );
        builder.add_instruction(
            entry,
            CfgInstruction::_AssignVar {
                var_id: *var_id,
                val: RValue::Value(v),
            },
        );
    }
    let mut semantic_errors = Vec::new();

    lower_stmt_to_block(
        &mut builder,
        &mut bb,
        None,
        body,
        sema,
        &mut semantic_errors,
        warnings,
    );

    if !semantic_errors.is_empty() {
        return Err(LowerError::SemanticErrors(semantic_errors));
    }

    let mut validate_errors = Vec::new();
    builder.link_successors_and_predecessors(&mut validate_errors);
    if !validate_errors.is_empty() {
        return Err(LowerError::CfgValidationErrors(validate_errors));
    }

    phi_insertion(&mut builder, sema);
    assignment_removal(&mut builder);

    let malformed = malformed_phi_reduction(&mut builder);

    if !malformed.malformed_assignments.is_empty() {
        return Err(LowerError::MalformedPhiInfo(malformed));
    }

    cleanup_passes(&mut builder);

    match builder.build(entry) {
        Ok(cfg) => Ok(cfg),
        Err(e) => Err(LowerError::CfgValidationErrors(e)),
    }
}

#[derive(Clone, Copy, Debug)]
struct ControlFlowLoopRefs {
    /// BB to jump to on continue
    continue_bb: BBId,
    /// BB to jump to on break
    exit_bb: BBId,
}

fn lower_stmt_to_block(
    builder: &mut CfgBuilder,
    bb_id: &mut BBId,
    cf_loop: Option<ControlFlowLoopRefs>,
    stmt: &Spanned<Stmt>,
    sema: &SemaResults,
    semantic_errors: &mut Vec<SemanticError>,
    warnings: &mut Vec<CfgWarning>,
) {
    // If we are in a basic block that already has a tail instruction, any further statements are unreachable
    // this can happen if we are in a block, and a previous statement was a return, break, or continue
    // in that case, we should emit a warning and skip lowering this statement
    if builder.bb_has_tail(*bb_id) {
        warnings.push(CfgWarning::UnreachableCode(stmt.span.clone()));
        return;
    }
    match &**stmt {
        Stmt::Expr(spanned) => {
            lower_expr(builder, bb_id, spanned, sema, semantic_errors);
        }
        Stmt::Block(spanneds) => {
            for s in spanneds {
                lower_stmt_to_block(builder, bb_id, cf_loop, s, sema, semantic_errors, warnings);
            }
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let mut then_bb = builder.create_bb();
            let (exit_bb, mut else_bb) = if let Some(_) = else_branch {
                let else_bb = builder.create_bb();
                let exit_bb = builder.create_bb();
                (exit_bb, else_bb)
            } else {
                let exit_bb = builder.create_bb();
                (exit_bb, exit_bb)
            };

            let cond_val = lower_expr(builder, bb_id, cond, sema, semantic_errors);
            builder.set_tail_instruction(
                *bb_id,
                TailCfgInstruction::CondBranch {
                    cond: cond_val,
                    then_bb,
                    else_bb,
                },
            );
            // Lower then branch
            lower_stmt_to_block(
                builder,
                &mut then_bb,
                cf_loop,
                then_branch,
                sema,
                semantic_errors,
                warnings,
            );
            if !builder.bb_has_tail(then_bb) {
                builder.set_tail_instruction(
                    then_bb,
                    TailCfgInstruction::UncondBranch { target: exit_bb },
                );
            }
            // Lower else branch
            if let Some(else_branch) = else_branch {
                lower_stmt_to_block(
                    builder,
                    &mut else_bb,
                    cf_loop,
                    else_branch,
                    sema,
                    semantic_errors,
                    warnings,
                );
                if !builder.bb_has_tail(else_bb) {
                    builder.set_tail_instruction(
                        else_bb,
                        TailCfgInstruction::UncondBranch { target: exit_bb },
                    );
                }
            }
            *bb_id = exit_bb;
        }
        Stmt::While { cond, body } => {
            let mut entry = builder.create_bb();
            let mut body_bb = builder.create_bb();
            let exit_bb = builder.create_bb();

            builder
                .set_tail_instruction(*bb_id, TailCfgInstruction::UncondBranch { target: entry });

            let cond_val = lower_expr(builder, &mut entry, cond, sema, semantic_errors);
            builder.set_tail_instruction(
                entry,
                TailCfgInstruction::CondBranch {
                    cond: cond_val,
                    then_bb: body_bb,
                    else_bb: exit_bb,
                },
            );
            let loop_refs = ControlFlowLoopRefs {
                continue_bb: entry,
                exit_bb,
            };
            // Lower body
            lower_stmt_to_block(
                builder,
                &mut body_bb,
                Some(loop_refs),
                body,
                sema,
                semantic_errors,
                warnings,
            );
            builder
                .set_tail_instruction(body_bb, TailCfgInstruction::UncondBranch { target: entry });
            *bb_id = exit_bb;
        }
        Stmt::For {
            init,
            cond,
            update,
            body,
        } => {
            if let Some(init_stmt) = init {
                lower_stmt_to_block(
                    builder,
                    bb_id,
                    cf_loop,
                    init_stmt,
                    sema,
                    semantic_errors,
                    warnings,
                );
            }

            let mut entry = builder.create_bb();
            let mut body_bb = builder.create_bb();
            let mut update_bb = builder.create_bb();
            let exit_bb = builder.create_bb();

            builder
                .set_tail_instruction(*bb_id, TailCfgInstruction::UncondBranch { target: entry });

            match cond {
                Some(cond_expr) => {
                    let cond_val =
                        lower_expr(builder, &mut entry, cond_expr, sema, semantic_errors);
                    builder.set_tail_instruction(
                        entry,
                        TailCfgInstruction::CondBranch {
                            cond: cond_val,
                            then_bb: body_bb,
                            else_bb: exit_bb,
                        },
                    );
                }
                None => {
                    builder.set_tail_instruction(
                        entry,
                        TailCfgInstruction::UncondBranch { target: body_bb },
                    );
                }
            }
            let loop_refs = ControlFlowLoopRefs {
                continue_bb: update_bb,
                exit_bb,
            };
            // Lower body
            lower_stmt_to_block(
                builder,
                &mut body_bb,
                Some(loop_refs),
                body,
                sema,
                semantic_errors,
                warnings,
            );
            // Lower update
            builder.set_tail_instruction(
                body_bb,
                TailCfgInstruction::UncondBranch { target: update_bb },
            );
            if let Some(update_expr) = update {
                lower_expr(builder, &mut update_bb, update_expr, sema, semantic_errors);
            }
            builder.set_tail_instruction(
                update_bb,
                TailCfgInstruction::UncondBranch { target: entry },
            );
            *bb_id = exit_bb;
        }
        Stmt::Return(spanned) => {
            let return_value = spanned.as_ref().map(|expr| {
                // Lower expr to value ref
                lower_expr(builder, bb_id, expr, sema, semantic_errors)
            });
            builder.set_tail_instruction(
                *bb_id,
                TailCfgInstruction::Return {
                    value: return_value,
                },
            );
        }
        Stmt::Break => {
            if let Some(cf_loop) = cf_loop {
                builder.set_tail_instruction(
                    *bb_id,
                    TailCfgInstruction::UncondBranch {
                        target: cf_loop.exit_bb,
                    },
                );
            } else {
                semantic_errors.push(SemanticError::BreakOutsideLoop);
            }
        }
        Stmt::Continue => {
            if let Some(cf_loop) = cf_loop {
                builder.set_tail_instruction(
                    *bb_id,
                    TailCfgInstruction::UncondBranch {
                        target: cf_loop.continue_bb,
                    },
                );
            } else {
                semantic_errors.push(SemanticError::ContinueOutsideLoop);
            }
        }
        Stmt::VarDecl {
            id,
            ty: _,
            name: _,
            init,
        } => {
            if let Some(init_expr) = init {
                let init_val = lower_expr(builder, bb_id, init_expr, sema, semantic_errors);
                builder.add_instruction(
                    *bb_id,
                    CfgInstruction::_AssignVar {
                        var_id: *id,
                        val: RValue::Value(init_val),
                    },
                );
            }
        }
    }
}

fn lower_expr(
    builder: &mut CfgBuilder,
    bb: &mut BBId,
    root: &Spanned<Expr>,
    sema: &SemaResults,
    semantic_errors: &mut Vec<SemanticError>,
) -> ValueRef {
    match &**root {
        Expr::IntLit(v) => {
            let val_ref = builder.allocate_value(root.span, None);
            builder.add_instruction(
                *bb,
                CfgInstruction::Assign {
                    dest: val_ref,
                    val: RValue::Const(*v),
                },
            );
            val_ref
        }
        Expr::Ident(id, ident) => {
            // Lookup variable in sema to get its current value ref
            let id = if let Some(var_id) = sema.lookup_ref_id(*id) {
                var_id
            } else {
                panic!(
                    "Variable {:?} ({}) not found in semantic analysis results",
                    id, ident
                );
            };

            let val_ref = builder.allocate_value(root.span, None);
            builder.add_instruction(
                *bb,
                CfgInstruction::Assign {
                    dest: val_ref,
                    val: RValue::_VarId(id),
                },
            );
            val_ref
        }
        Expr::BinOp { op, left, right } => match &**op {
            BinaryOp::Add => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::Add {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::Subtract => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::Sub {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::Multiply => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::Mul {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::Divide => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::Div {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::Modulus => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::Modulus {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::Assign => {
                let left_ident = if let Expr::Ident(id, _) = &***left {
                    *id
                } else {
                    panic!("Left-hand side of assignment must be an identifier");
                };
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                // Lookup variable in sema to get its current value ref
                let var_id = if let Some(var_id) = sema.lookup_ref_id(left_ident) {
                    var_id
                } else {
                    panic!(
                        "Variable {:?} not found in semantic analysis results",
                        left_ident
                    );
                };
                builder.add_instruction(
                    *bb,
                    CfgInstruction::_AssignVar {
                        var_id,
                        val: RValue::Value(right_val),
                    },
                );
                right_val
            }
            BinaryOp::AddAssign => {
                let left_ident = if let Expr::Ident(id, _) = &***left {
                    *id
                } else {
                    panic!("Left-hand side of assignment must be an identifier");
                };

                let left_var_id = if let Some(var_id) = sema.lookup_ref_id(left_ident) {
                    var_id
                } else {
                    panic!(
                        "Variable {:?} not found in semantic analysis results",
                        left_ident
                    );
                };

                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, Some(left_var_id));
                let left_val =
                    builder.allocate_value(sema.var_span(left_var_id), Some(left_var_id));
                // Load current value of left variable
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: left_val,
                        val: RValue::_VarId(left_var_id),
                    },
                );
                // Perform addition
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::Add {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                // Store back to variable
                builder.add_instruction(
                    *bb,
                    CfgInstruction::_AssignVar {
                        var_id: left_var_id,
                        val: RValue::Value(result_val),
                    },
                );
                result_val
            }
            BinaryOp::SubtractAssign => {
                let left_ident = if let Expr::Ident(id, _) = &***left {
                    *id
                } else {
                    panic!("Left-hand side of assignment must be an identifier");
                };

                let left_var_id = if let Some(var_id) = sema.lookup_ref_id(left_ident) {
                    var_id
                } else {
                    panic!(
                        "Variable {:?} not found in semantic analysis results",
                        left_ident
                    );
                };

                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, Some(left_var_id));
                let left_val =
                    builder.allocate_value(sema.var_span(left_var_id), Some(left_var_id));
                // Load current value of left variable
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: left_val,
                        val: RValue::_VarId(left_var_id),
                    },
                );
                // Perform addition
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::Sub {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                // Store back to variable
                builder.add_instruction(
                    *bb,
                    CfgInstruction::_AssignVar {
                        var_id: left_var_id,
                        val: RValue::Value(result_val),
                    },
                );
                result_val
            }
            BinaryOp::MultiplyAssign => {
                let left_ident = if let Expr::Ident(id, _) = &***left {
                    *id
                } else {
                    panic!("Left-hand side of assignment must be an identifier");
                };

                let left_var_id = if let Some(var_id) = sema.lookup_ref_id(left_ident) {
                    var_id
                } else {
                    panic!(
                        "Variable {:?} not found in semantic analysis results",
                        left_ident
                    );
                };

                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, Some(left_var_id));
                let left_val =
                    builder.allocate_value(sema.var_span(left_var_id), Some(left_var_id));
                // Load current value of left variable
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: left_val,
                        val: RValue::_VarId(left_var_id),
                    },
                );
                // Perform addition
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::Mul {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                // Store back to variable
                builder.add_instruction(
                    *bb,
                    CfgInstruction::_AssignVar {
                        var_id: left_var_id,
                        val: RValue::Value(result_val),
                    },
                );
                result_val
            }
            BinaryOp::DivideAssign => {
                let left_ident = if let Expr::Ident(id, _) = &***left {
                    *id
                } else {
                    panic!("Left-hand side of assignment must be an identifier");
                };

                let left_var_id = if let Some(var_id) = sema.lookup_ref_id(left_ident) {
                    var_id
                } else {
                    panic!(
                        "Variable {:?} not found in semantic analysis results",
                        left_ident
                    );
                };

                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, Some(left_var_id));
                let left_val =
                    builder.allocate_value(sema.var_span(left_var_id), Some(left_var_id));
                // Load current value of left variable
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: left_val,
                        val: RValue::_VarId(left_var_id),
                    },
                );
                // Perform addition
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::Div {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                // Store back to variable
                builder.add_instruction(
                    *bb,
                    CfgInstruction::_AssignVar {
                        var_id: left_var_id,
                        val: RValue::Value(result_val),
                    },
                );
                result_val
            }
            BinaryOp::ModulusAssign => {
                let left_ident = if let Expr::Ident(id, _) = &***left {
                    *id
                } else {
                    panic!("Left-hand side of assignment must be an identifier");
                };

                let left_var_id = if let Some(var_id) = sema.lookup_ref_id(left_ident) {
                    var_id
                } else {
                    panic!(
                        "Variable {:?} not found in semantic analysis results",
                        left_ident
                    );
                };

                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, Some(left_var_id));
                let left_val =
                    builder.allocate_value(sema.var_span(left_var_id), Some(left_var_id));
                // Load current value of left variable
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: left_val,
                        val: RValue::_VarId(left_var_id),
                    },
                );
                // Perform addition
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::Modulus {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                // Store back to variable
                builder.add_instruction(
                    *bb,
                    CfgInstruction::_AssignVar {
                        var_id: left_var_id,
                        val: RValue::Value(result_val),
                    },
                );
                result_val
            }
            BinaryOp::Equal => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::EqCheck {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::NotEqual => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::NEqCheck {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::LessThan => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::LtCheck {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::GreaterThan => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::GtCheck {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::LessThanOrEqual => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::LEqCheck {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::GreaterThanOrEqual => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::GEqCheck {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::And => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let mut right_eval_block = builder.create_bb();
                let exit_bb = builder.create_bb();
                let result_val = builder.allocate_value(root.span, None);
                builder.set_tail_instruction(
                    *bb,
                    TailCfgInstruction::CondBranch {
                        cond: left_val,
                        then_bb: right_eval_block,
                        else_bb: exit_bb,
                    },
                );
                // Lower right expression in its own block
                let right_val =
                    lower_expr(builder, &mut right_eval_block, right, sema, semantic_errors);
                builder.set_tail_instruction(
                    right_eval_block,
                    TailCfgInstruction::UncondBranch { target: exit_bb },
                );
                // In exit block, create phi instruction to select correct value
                builder.add_phi_instruction(
                    exit_bb,
                    result_val,
                    [
                        (*bb, ValueRefOrConst::Const(0)), // false if left was false
                        (right_eval_block, ValueRefOrConst::Value(right_val)), // right value
                    ]
                    .into_iter()
                    .collect(),
                    None,
                );
                *bb = exit_bb;

                result_val
            }
            BinaryOp::Or => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let mut right_eval_block = builder.create_bb();
                let exit_bb = builder.create_bb();
                let result_val = builder.allocate_value(root.span, None);
                builder.set_tail_instruction(
                    *bb,
                    TailCfgInstruction::CondBranch {
                        cond: left_val,
                        then_bb: exit_bb,
                        else_bb: right_eval_block,
                    },
                );
                // Lower right expression in its own block
                let right_val =
                    lower_expr(builder, &mut right_eval_block, right, sema, semantic_errors);
                builder.set_tail_instruction(
                    right_eval_block,
                    TailCfgInstruction::UncondBranch { target: exit_bb },
                );
                // In exit block, create phi instruction to select correct value
                builder.add_phi_instruction(
                    exit_bb,
                    result_val,
                    [
                        (*bb, ValueRefOrConst::Const(1)), // true if left was true
                        (right_eval_block, ValueRefOrConst::Value(right_val)), // right value
                    ]
                    .into_iter()
                    .collect(),
                    None,
                );
                *bb = exit_bb;

                result_val
            }
            BinaryOp::BitwiseAnd => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::BitwiseAnd {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::BitwiseOr => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::BitwiseOr {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::BitwiseXor => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::BitwiseXor {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::LeftShift => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::BitShiftLeft {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
            BinaryOp::RightShift => {
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
                let result_val = builder.allocate_value(root.span, None);
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: result_val,
                        val: RValue::BitShiftRight {
                            left: left_val,
                            right: right_val,
                        },
                    },
                );
                result_val
            }
        },
        Expr::UnaryOp { op, expr, prefix } => {
            match &**op {
                UnaryOp::Plus => lower_expr(builder, bb, expr, sema, semantic_errors),
                UnaryOp::Minus => {
                    let expr_val = lower_expr(builder, bb, expr, sema, semantic_errors);
                    let zero = builder.allocate_value(op.span, None);
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::Assign {
                            dest: zero,
                            val: RValue::Const(0),
                        },
                    );
                    let result_val = builder.allocate_value(root.span, None);
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::Assign {
                            dest: result_val,
                            val: RValue::Sub {
                                left: zero,
                                right: expr_val,
                            },
                        },
                    );
                    result_val
                }
                UnaryOp::Not => {
                    let expr_val = lower_expr(builder, bb, expr, sema, semantic_errors);
                    let result_val = builder.allocate_value(root.span, None);
                    let zero = builder.allocate_value(op.span, None);

                    builder.add_instruction(
                        *bb,
                        CfgInstruction::Assign {
                            dest: zero,
                            val: RValue::Const(0),
                        },
                    );
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::Assign {
                            dest: result_val,
                            val: RValue::EqCheck {
                                left: expr_val,
                                right: zero,
                            },
                        },
                    );
                    result_val
                }
                UnaryOp::BitwiseNot => {
                    let expr_val = lower_expr(builder, bb, expr, sema, semantic_errors);
                    let result_val = builder.allocate_value(root.span, None);
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::Assign {
                            dest: result_val,
                            val: RValue::BitwiseNot { expr: expr_val },
                        },
                    );
                    result_val
                }
                UnaryOp::Increment => {
                    let ident = if let Expr::Ident(id, _) = &***expr {
                        *id
                    } else {
                        panic!("Operand of increment must be an identifier");
                    };
                    let var_id = if let Some(var_id) = sema.lookup_ref_id(ident) {
                        var_id
                    } else {
                        panic!(
                            "Variable {:?} not found in semantic analysis results",
                            ident
                        );
                    };
                    let current_val = builder.allocate_value(expr.span, None);
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::Assign {
                            dest: current_val,
                            val: RValue::_VarId(var_id),
                        },
                    );
                    let one = builder.allocate_value(op.span, None);
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::Assign {
                            dest: one,
                            val: RValue::Const(1),
                        },
                    );
                    let incremented_val = builder.allocate_value(root.span, None);
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::Assign {
                            dest: incremented_val,
                            val: RValue::Add {
                                left: current_val,
                                right: one,
                            },
                        },
                    );
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::_AssignVar {
                            var_id,
                            val: RValue::Value(incremented_val),
                        },
                    );
                    if *prefix {
                        // ++i
                        incremented_val
                    } else {
                        // i++
                        current_val
                    }
                }
                UnaryOp::Decrement => {
                    let ident = if let Expr::Ident(id, _) = &***expr {
                        *id
                    } else {
                        panic!("Operand of decrement must be an identifier");
                    };
                    let var_id = if let Some(var_id) = sema.lookup_ref_id(ident) {
                        var_id
                    } else {
                        panic!(
                            "Variable {:?} not found in semantic analysis results",
                            ident
                        );
                    };
                    let current_val = builder.allocate_value(expr.span, None);
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::Assign {
                            dest: current_val,
                            val: RValue::_VarId(var_id),
                        },
                    );
                    let one = builder.allocate_value(op.span, None);
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::Assign {
                            dest: one,
                            val: RValue::Const(1),
                        },
                    );
                    let decremented_val = builder.allocate_value(root.span, None);
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::Assign {
                            dest: decremented_val,
                            val: RValue::Sub {
                                left: current_val,
                                right: one,
                            },
                        },
                    );
                    builder.add_instruction(
                        *bb,
                        CfgInstruction::_AssignVar {
                            var_id,
                            val: RValue::Value(decremented_val),
                        },
                    );
                    if *prefix {
                        // --i
                        decremented_val
                    } else {
                        // i--
                        current_val
                    }
                }
            }
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            let exit_bb = builder.create_bb();
            let mut then_bb = builder.create_bb();
            let mut else_bb = builder.create_bb();

            let cond_val = lower_expr(builder, bb, cond, sema, semantic_errors);
            builder.set_tail_instruction(
                *bb,
                TailCfgInstruction::CondBranch {
                    cond: cond_val,
                    then_bb,
                    else_bb,
                },
            );
            // Lower then branch
            let then_val = lower_expr(builder, &mut then_bb, then_expr, sema, semantic_errors);
            builder.set_tail_instruction(
                then_bb,
                TailCfgInstruction::UncondBranch { target: exit_bb },
            );
            // Lower else branch
            let else_val = lower_expr(builder, &mut else_bb, else_expr, sema, semantic_errors);
            builder.set_tail_instruction(
                else_bb,
                TailCfgInstruction::UncondBranch { target: exit_bb },
            );
            // Create phi instruction in exit bb
            let result_val = builder.allocate_value(root.span, None);
            builder.add_phi_instruction(
                exit_bb,
                result_val,
                [
                    (then_bb, ValueRefOrConst::Value(then_val)),
                    (else_bb, ValueRefOrConst::Value(else_val)),
                ]
                .into_iter()
                .collect(),
                None,
            );
            *bb = exit_bb;
            result_val
        }
        Expr::Call { func, args } => {
            let _func_val = lower_expr(builder, bb, func, sema, semantic_errors);
            let mut arg_vals = Vec::new();
            for arg in args {
                let arg_val = lower_expr(builder, bb, arg, sema, semantic_errors);
                arg_vals.push(arg_val);
            }
            let result_val = builder.allocate_value(root.span, None);
            builder.add_instruction(
                *bb,
                CfgInstruction::Assign {
                    dest: result_val,
                    val: RValue::Call {
                        func: _func_val,
                        args: arg_vals,
                    },
                },
            );
            result_val
        }
    }
}

fn phi_insertion(builder: &mut CfgBuilder, sema: &SemaResults) {
    let mut before_vars: BTreeMap<VarId, BTreeMap<BBId, ValueRef>> = BTreeMap::new();
    let mut after_vars: BTreeMap<VarId, BTreeMap<BBId, ValueRef>> = BTreeMap::new();
    let mut phis: BTreeMap<BBId, BTreeMap<VarId, BTreeMap<BBId, ValueRefOrConst>>> =
        BTreeMap::new();

    for bb in &mut builder.blocks {
        let mut during_vars = BTreeMap::<VarId, ValueRef>::new();
        for instr in &mut bb.instructions {
            match instr {
                CfgInstruction::_AssignVar { var_id, val } => {
                    let val_ref = match val {
                        RValue::Value(v) => *v,
                        _ => unreachable!(),
                    };
                    during_vars.insert(*var_id, val_ref);
                }
                CfgInstruction::Assign { dest: _, val } => {
                    if let RValue::_VarId(var_id) = val {
                        // attempt to replace with assigned value from same basic block if available
                        *val = during_vars
                            .get(var_id)
                            .map(|v| RValue::Value(*v))
                            .unwrap_or(RValue::_VarId(*var_id));
                    }
                }
            }
        }

        for (var_id, val_ref) in &during_vars {
            after_vars
                .entry(*var_id)
                .or_default()
                .insert(bb.id, *val_ref);
        }

        for next_bb in &bb.successors {
            let t = phis.entry(*next_bb).or_default();
            for (var_id, val_ref) in &during_vars {
                t.entry(*var_id)
                    .or_default()
                    .insert(bb.id, ValueRefOrConst::Value(*val_ref));
            }
        }
    }

    for (block, phi) in phis {
        for (var_id, sources) in phi {
            let (v, _) = builder.add_phi_source(block, var_id, sources, sema.var_span(var_id));
            before_vars.entry(var_id).or_default().insert(block, v);
        }
    }

    loop {
        let mut changed = false;

        let mut phis_to_add: BTreeMap<BBId, BTreeMap<VarId, BTreeMap<BBId, ValueRefOrConst>>> =
            BTreeMap::new();

        for bb in &mut builder.blocks {
            let before = before_vars
                .iter()
                .filter_map(|(var_id, vals)| vals.get(&bb.id).map(|v| (*var_id, *v)))
                .collect::<BTreeMap<VarId, ValueRef>>();
            let after = after_vars
                .iter()
                .filter_map(|(var_id, vals)| vals.get(&bb.id).map(|v| (*var_id, *v)))
                .collect::<BTreeMap<VarId, ValueRef>>();
            // save variables to propagate from before to after
            for v in &before {
                if !after.contains_key(v.0) {
                    let val_ref = before_vars[v.0][&bb.id];
                    after_vars.entry(*v.0).or_default().insert(bb.id, val_ref);

                    for succ_bb in &bb.successors {
                        phis_to_add
                            .entry(*succ_bb)
                            .or_default()
                            .entry(*v.0)
                            .or_default()
                            .insert(bb.id, ValueRefOrConst::Value(val_ref));
                    }
                }
            }
        }

        // propagate variables
        for (block, phi) in phis_to_add {
            for (var_id, sources) in phi {
                let (v, ch) = builder.add_phi_source(block, var_id, sources, sema.var_span(var_id));
                before_vars.entry(var_id).or_default().insert(block, v);
                changed |= ch;
            }
        }

        if !changed {
            break;
        }
    }

    // finally, attempt to replace _VarId uses with the correct ValueRef from before_vars
    for bb in &mut builder.blocks {
        for instr in &mut bb.instructions {
            if let CfgInstruction::Assign { dest: _, val } = instr
                && let RValue::_VarId(var_id) = val
            {
                if let Some(v) = before_vars.get(var_id).and_then(|m| m.get(&bb.id)) {
                    *val = RValue::Value(*v);
                } else {
                    // panic!(
                    //     "Could not find value for variable {:?} in basic block {:?}",
                    //     var_id, bb.id
                    // );
                }
            }
        }
    }
}

fn assignment_removal(builder: &mut CfgBuilder) {
    // pass happens after phi insertion to remove all _AssignVar instructions
    for bb in &mut builder.blocks {
        bb.instructions.retain(|instr| {
            if let CfgInstruction::_AssignVar { var_id: _, val: _ } = instr {
                false
            } else {
                true
            }
        });
    }
}

#[derive(Debug)]
pub struct SingleMalformedPhiInfo {
    pub bb_id: BBId,
    pub phi_var_id: Option<VarId>,
    pub sources: BTreeMap<BBId, ValueRefOrConst>,
}

#[derive(Debug)]
pub struct SingleMalformedAssignmentInfo {
    pub bb_id: BBId,
    pub dest_val: ValueRef,
    pub assigned_val: RValue,
}

impl SemanticsAwareIntoDiagnostic for SingleMalformedAssignmentInfo {
    fn to_diagnostic_with_sema(&self, file_id: usize, sema: &SemaResults) -> Diagnostic<usize> {
        let (decl_site, prim_label_text, note_text) = match &self.assigned_val {
            RValue::Value(v) => (
                v.1.range(),
                "value not valid due to prior malformed phi",
                "note: this means that there are assignments to this variable, but not on all possible code paths",
            ),
            RValue::_VarId(v) => (
                sema.var_span(*v).range(),
                "value not valid (not yet initialized)",
                "note: this means that you are trying to use a variable that has not been assigned a value yet",
            ),
            _ => unreachable!(),
        };

        let diag = Diagnostic::error()
            .with_message("Use before initialization detected during CFG lowering")
            .with_label(
                Label::primary(file_id, self.dest_val.1.range()).with_message(prim_label_text),
            )
            .with_label(
                Label::secondary(file_id, decl_site).with_message("Variable declaration site"),
            )
            .with_note(note_text);
        diag
    }
}

#[derive(Debug)]
pub struct MalformedPhiInfo {
    pub infos: Vec<SingleMalformedPhiInfo>,
    pub malformed_assignments: Vec<SingleMalformedAssignmentInfo>,
}

fn malformed_phi_reduction(builder: &mut CfgBuilder) -> MalformedPhiInfo {
    // remove any phi instructions that have sources not matching the predecessors of the basic block
    let mut infos = Vec::new();
    let mut removed_vals = BTreeSet::new();
    let mut malformed_assignments = Vec::new();
    loop {
        let mut changed = false;
        for bb in &mut builder.blocks {
            let malformed_phis = bb
                .phi
                .extract_if(0..bb.phi.len(), |phi| {
                    phi.sources.keys().collect::<BTreeSet<&BBId>>()
                        != bb.predecessors.iter().collect::<BTreeSet<&BBId>>()
                        || phi.sources.iter().any(|s| match s.1 {
                            ValueRefOrConst::Value(v) => removed_vals.contains(v),
                            _ => false,
                        })
                })
                .collect::<Vec<_>>();
            for phi in malformed_phis {
                removed_vals.insert(phi.dest);
                infos.push(SingleMalformedPhiInfo {
                    bb_id: bb.id,
                    phi_var_id: phi.var_id,
                    sources: phi.sources,
                });
                changed = true;
            }
            let malformed_instrs = bb
                .instructions
                .extract_if(0..bb.instructions.len(), |instr| {
                    if let CfgInstruction::Assign { dest: _, val } = instr {
                        match val {
                            RValue::Value(v) => removed_vals.contains(v),
                            RValue::_VarId(_) => true,
                            _ => false,
                        }
                    } else {
                        false
                    }
                })
                .collect::<Vec<_>>();
            for instr in malformed_instrs {
                removed_vals.insert(match instr {
                    CfgInstruction::Assign {
                        dest,
                        val: RValue::Value(_),
                    } => dest,
                    CfgInstruction::Assign {
                        dest,
                        val: RValue::_VarId(_),
                    } => dest,
                    _ => unreachable!(),
                });
                if let CfgInstruction::Assign { dest, val } = instr {
                    malformed_assignments.push(SingleMalformedAssignmentInfo {
                        bb_id: bb.id,
                        dest_val: dest,
                        assigned_val: val.clone(),
                    });
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    MalformedPhiInfo {
        infos,
        malformed_assignments,
    }
}

fn phi_simplification(builder: &mut CfgBuilder) -> bool {
    // remove any phi instructions that have all sources the same
    let mut to_add = Vec::new();
    for bb in &mut builder.blocks {
        let simplified_phis = bb
            .phi
            .extract_if(0..bb.phi.len(), |phi| {
                let first_source = phi.sources.values().next().unwrap();
                phi.sources.values().all(|v| v == first_source)
            })
            .collect::<Vec<_>>();
        for phi in simplified_phis {
            let first_source = phi.sources.values().next().unwrap();
            let instr = CfgInstruction::Assign {
                dest: phi.dest,
                val: match first_source {
                    ValueRefOrConst::Value(v) => RValue::Value(*v),
                    ValueRefOrConst::Const(c) => RValue::Const(*c),
                },
            };
            to_add.push((bb.id, instr));
        }
    }

    let changed = !to_add.is_empty();

    for (bb_id, instr) in to_add {
        builder.prepend_instruction(bb_id, instr);
    }

    changed
}

fn val_inliner(builder: &mut CfgBuilder) -> bool {
    // inline values that are assigned only once and used only once
    let mut equalities = BTreeMap::<ValueRef, ValueRef>::new();
    for bb in &builder.blocks {
        for instr in &bb.instructions {
            if let CfgInstruction::Assign { dest, val } = instr
                && let RValue::Value(v) = val
            {
                equalities.insert(*dest, *v);
            }
        }
    }

    let mut changed = false;

    for bb in &mut builder.blocks {
        for phi in &mut bb.phi {
            for source in phi.sources.values_mut() {
                if let ValueRefOrConst::Value(v) = source {
                    let mut current = *v;
                    while let Some(next) = equalities.get(&current) {
                        current = *next;
                    }
                    if v != &current {
                        *v = current;
                        changed = true;
                    }
                }
            }
        }
        for instr in &mut bb.instructions {
            if let CfgInstruction::Assign { dest: _, val } = instr {
                match val {
                    RValue::Value(v) => {
                        let mut current = *v;
                        while let Some(next) = equalities.get(&current) {
                            current = *next;
                        }
                        if v != &current {
                            *v = current;
                            changed = true;
                        }
                    }
                    RValue::Add { left, right }
                    | RValue::Sub { left, right }
                    | RValue::Mul { left, right }
                    | RValue::Div { left, right }
                    | RValue::Modulus { left, right }
                    | RValue::EqCheck { left, right }
                    | RValue::NEqCheck { left, right }
                    | RValue::LtCheck { left, right }
                    | RValue::GtCheck { left, right }
                    | RValue::LEqCheck { left, right }
                    | RValue::GEqCheck { left, right }
                    | RValue::BitwiseAnd { left, right }
                    | RValue::BitwiseOr { left, right }
                    | RValue::BitwiseXor { left, right }
                    | RValue::BitShiftLeft { left, right }
                    | RValue::BitShiftRight { left, right } => {
                        let mut current_left = *left;
                        while let Some(next) = equalities.get(&current_left) {
                            current_left = *next;
                        }
                        if left != &current_left {
                            *left = current_left;
                            changed = true;
                        }
                        let mut current_right = *right;
                        while let Some(next) = equalities.get(&current_right) {
                            current_right = *next;
                        }
                        if right != &current_right {
                            *right = current_right;
                            changed = true;
                        }
                    }
                    RValue::Call { func, args } => {
                        let mut current_func = *func;
                        while let Some(next) = equalities.get(&current_func) {
                            current_func = *next;
                        }
                        if func != &current_func {
                            *func = current_func;
                            changed = true;
                        }
                        for arg in args {
                            let mut current_arg = *arg;
                            while let Some(next) = equalities.get(&current_arg) {
                                current_arg = *next;
                            }
                            if arg != &current_arg {
                                *arg = current_arg;
                                changed = true;
                            }
                        }
                    }
                    RValue::Const(_) => {}
                    RValue::Param { .. } => {}
                    RValue::Function { .. } => {}
                    RValue::_VarId(_) => {}
                    RValue::BitwiseNot { expr } => {
                        let mut current_expr = *expr;
                        while let Some(next) = equalities.get(&current_expr) {
                            current_expr = *next;
                        }
                        if expr != &current_expr {
                            *expr = current_expr;
                            changed = true;
                        }
                    }
                }
            }
        }

        match &mut bb.tail {
            TailCfgInstruction::CondBranch {
                cond,
                then_bb: _,
                else_bb: _,
            } => {
                let mut current = *cond;
                while let Some(next) = equalities.get(&current) {
                    current = *next;
                }
                if cond != &current {
                    *cond = current;
                    changed = true;
                }
            }
            TailCfgInstruction::UncondBranch { target: _ } => {}
            TailCfgInstruction::Return { value } => {
                if let Some(v) = value {
                    let mut current = *v;
                    while let Some(next) = equalities.get(&current) {
                        current = *next;
                    }
                    if v != &current {
                        *v = current;
                        changed = true;
                    }
                }
            }
            TailCfgInstruction::Undefined => {
                unreachable!()
            }
        }
    }

    changed
}

fn live_values_analysis(builder: &mut CfgBuilder) -> BTreeSet<ValueRef> {
    let mut live_values = BTreeSet::new();

    loop {
        let mut changed = false;

        for bb in &builder.blocks {
            for phi in &bb.phi {
                for source in phi.sources.values() {
                    match source {
                        ValueRefOrConst::Value(v) => {
                            if !live_values.contains(v) {
                                live_values.insert(*v);
                                changed = true;
                            }
                        }
                        ValueRefOrConst::Const(_) => {}
                    }
                }
            }
            for instr in bb.instructions.iter().rev() {
                match instr {
                    CfgInstruction::Assign { dest, val } => {
                        if matches!(val, RValue::Call { .. }) {
                            // function calls are assumed to have side effects, so their dest is always live
                            if !live_values.contains(dest) {
                                live_values.insert(*dest);
                                changed = true;
                            }
                        }

                        if !live_values.contains(dest) {
                            continue;
                        }

                        match val {
                            RValue::Value(v) => {
                                if !live_values.contains(v) {
                                    live_values.insert(*v);
                                    changed = true;
                                }
                            }
                            RValue::Const(_) => {}
                            RValue::Add { left, right }
                            | RValue::Sub { left, right }
                            | RValue::Mul { left, right }
                            | RValue::Div { left, right }
                            | RValue::Modulus { left, right }
                            | RValue::EqCheck { left, right }
                            | RValue::NEqCheck { left, right }
                            | RValue::LtCheck { left, right }
                            | RValue::GtCheck { left, right }
                            | RValue::LEqCheck { left, right }
                            | RValue::GEqCheck { left, right }
                            | RValue::BitwiseAnd { left, right }
                            | RValue::BitwiseOr { left, right }
                            | RValue::BitwiseXor { left, right }
                            | RValue::BitShiftLeft { left, right }
                            | RValue::BitShiftRight { left, right } => {
                                if !live_values.contains(left) {
                                    live_values.insert(*left);
                                    changed = true;
                                }
                                if !live_values.contains(right) {
                                    live_values.insert(*right);
                                    changed = true;
                                }
                            }
                            RValue::BitwiseNot { expr } => {
                                if !live_values.contains(expr) {
                                    live_values.insert(*expr);
                                    changed = true;
                                }
                            }
                            RValue::Call { func, args } => {
                                if !live_values.contains(func) {
                                    live_values.insert(*func);
                                    changed = true;
                                }
                                for arg in args {
                                    if !live_values.contains(arg) {
                                        live_values.insert(*arg);
                                        changed = true;
                                    }
                                }
                            }
                            RValue::_VarId(..) => {
                                // unreachable!()
                            }
                            RValue::Param { .. } => {}
                            RValue::Function { .. } => {}
                        }
                    }
                    CfgInstruction::_AssignVar { .. } => {
                        unreachable!()
                    }
                }
            }

            match &bb.tail {
                TailCfgInstruction::CondBranch {
                    cond,
                    then_bb: _,
                    else_bb: _,
                } => {
                    if !live_values.contains(cond) {
                        live_values.insert(*cond);
                        changed = true;
                    }
                }
                TailCfgInstruction::UncondBranch { target: _ } => {}
                TailCfgInstruction::Return { value } => {
                    if let Some(v) = value {
                        if !live_values.contains(v) {
                            live_values.insert(*v);
                            changed = true;
                        }
                    }
                }
                TailCfgInstruction::Undefined => {
                    unreachable!()
                }
            }
        }

        if !changed {
            break;
        }
    }

    live_values
}

fn dead_value_elimination(builder: &mut CfgBuilder) {
    let live_values = live_values_analysis(builder);

    for bb in &mut builder.blocks {
        bb.instructions.retain(|instr| {
            if let CfgInstruction::Assign { dest, val: _ } = instr
                && !live_values.contains(dest)
            {
                false
            } else {
                true
            }
        });
    }
}

fn constant_propagation(builder: &mut CfgBuilder) {
    let mut constants = BTreeMap::<ValueRef, i64>::new();

    loop {
        let mut changed = false;

        for bb in &builder.blocks {
            for phi in &bb.phi {
                let first_source = phi.sources.values().next().unwrap();
                match first_source {
                    ValueRefOrConst::Value(v) => {
                        if let Some(c) = constants.get(v) {
                            if phi.sources.values().all(|v| match v {
                                ValueRefOrConst::Value(v2) => {
                                    if let Some(c2) = constants.get(v2) {
                                        c == c2
                                    } else {
                                        false
                                    }
                                }
                                ValueRefOrConst::Const(c2) => c == c2,
                            }) {
                                if !constants.contains_key(&phi.dest) || constants[&phi.dest] != *c
                                {
                                    constants.insert(phi.dest, *c);
                                    changed = true;
                                }
                            }
                        }
                    }
                    ValueRefOrConst::Const(c) => {
                        if phi.sources.values().all(|v| match v {
                            ValueRefOrConst::Const(c2) => c == c2,
                            ValueRefOrConst::Value(v2) => match constants.get(v2) {
                                Some(c2) => c == c2,
                                None => false,
                            },
                        }) {
                            if !constants.contains_key(&phi.dest) || constants[&phi.dest] != *c {
                                constants.insert(phi.dest, *c);
                                changed = true;
                            }
                        }
                    }
                }
            }

            for instr in &bb.instructions {
                if let CfgInstruction::Assign { dest, val } = instr {
                    match val {
                        RValue::Const(c) => {
                            if !constants.contains_key(dest) || constants[dest] != *c {
                                constants.insert(*dest, *c);
                                changed = true;
                            }
                        }
                        RValue::Add { left, right } => {
                            if let (Some(lc), Some(rc)) =
                                (constants.get(left), constants.get(right))
                            {
                                let result = lc + rc;
                                if !constants.contains_key(dest) || constants[dest] != result {
                                    constants.insert(*dest, result);
                                    changed = true;
                                }
                            }
                        }
                        RValue::Sub { left, right } => {
                            if let (Some(lc), Some(rc)) =
                                (constants.get(left), constants.get(right))
                            {
                                let result = lc - rc;
                                if !constants.contains_key(dest) || constants[dest] != result {
                                    constants.insert(*dest, result);
                                    changed = true;
                                }
                            }
                        }
                        RValue::Mul { left, right } => {
                            if let (Some(lc), Some(rc)) =
                                (constants.get(left), constants.get(right))
                            {
                                let result = lc * rc;
                                if !constants.contains_key(dest) || constants[dest] != result {
                                    constants.insert(*dest, result);
                                    changed = true;
                                }
                            }
                        }
                        RValue::Div { left, right } => {
                            if let (Some(lc), Some(rc)) =
                                (constants.get(left), constants.get(right))
                            {
                                if *rc != 0 {
                                    let result = lc / rc;
                                    if !constants.contains_key(dest) || constants[dest] != result {
                                        constants.insert(*dest, result);
                                        changed = true;
                                    }
                                }
                            }
                        }
                        RValue::Modulus { left, right } => {
                            if let (Some(lc), Some(rc)) =
                                (constants.get(left), constants.get(right))
                            {
                                if *rc != 0 {
                                    let result = lc % rc;
                                    if !constants.contains_key(dest) || constants[dest] != result {
                                        constants.insert(*dest, result);
                                        changed = true;
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        if !changed {
            break;
        }
    }

    for bb in &mut builder.blocks {
        for phi in &mut bb.phi {
            for s in phi.sources.values_mut() {
                if let ValueRefOrConst::Value(v) = s {
                    if let Some(c) = constants.get(v) {
                        *s = ValueRefOrConst::Const(*c);
                    }
                }
            }
        }
        for instr in &mut bb.instructions {
            if let CfgInstruction::Assign { dest, val } = instr {
                if let Some(c) = constants.get(dest) {
                    *val = RValue::Const(*c);
                }
            }
        }
    }
}

fn cleanup_passes(builder: &mut CfgBuilder) {
    loop {
        let mut changed = false;
        changed |= phi_simplification(builder);
        changed |= val_inliner(builder);
        if !changed {
            break;
        }
    }

    constant_propagation(builder);
    dead_value_elimination(builder);
}
