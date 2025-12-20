//! Lowering of AST to Control Flow Graph (CFG) structures.
//!
//! This file contains the main logic for lowering high-level AST constructs into a
//! control flow graph representation suitable for further analysis and optimization.
//!
//! The lowering process involves translating statements and expressions into
//! basic blocks and instructions, handling control flow constructs like
//! if statements and loops, and inserting phi nodes where necessary.
//!
//! At the end of the construction, we also perform several cleanup passes to
//! optimize the CFG.
//!
//! The main entry point is the `lower_ast_to_cfg` function, which takes an AST
//! function and semantic analysis results, and produces a CFG.
//!
//! After the CFG is constructed, we insert phi nodes at all merge points (see [`phi_insertion`]),
//! then we perform validation to ensure the CFG is well-formed (mainly that all PHI nodes are
//! correctly formed, see [`malformed_phi_reduction`]).
//!
//! Error handling is performed throughout the lowering process, with
//! specific error types defined for various failure modes.
//!
//! The resulting CFG can then be used for further analysis or code generation.
//!
//! For the code generation phase, we need it to follow the following invariant:
//!
//! No back edges should exist in the CFG except those created by loops.
//! (The order of basic blocks in the CFG should be a topological sort of the CFG graph modulo
//! back edges created by loops.)
//!
//! This is necessary to ensure that our code generation algorithm can correctly generate
//! the LLVM IR from the CFG.
//!
//! This mostly means that we try to have the blocks in the same order in the CFG as they
//! appear in the source code, except for loops which may create back edges.
//!
//! This is to allow our lowering to LLVM IR in O(n) time without needing to do complex
//! analysis of the CFG structure.

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Write as _,
};

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::{
    cfg::{
        builder::{CfgBuilder, ValidateError},
        cleanup::{OptPassConfig, cleanup_passes},
        def::*,
        sema::SemaResults,
    },
    diagnostics::{IntoDiagnostic, SemanticsAwareIntoDiagnostic},
    parser::{
        Expr, Function, Stmt,
        ast::{Block, VarId},
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
    ast: &Function,
    pass_config: &OptPassConfig,
    sema: &SemaResults,
    warnings: &mut Vec<CfgWarning>,
) -> Result<ControlFlowGraph, LowerError> {
    let (mut builder, entry) = CfgBuilder::new();

    let Some(body) = &ast.body else {
        return Err(LowerError::NoBody);
    };

    let mut bb = entry;

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

    lower_block_to_block(
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
    builder.trim_dead_blocks();

    resolve_function_refs(&mut builder, sema);

    phi_insertion(&mut builder, sema);
    assignment_removal(&mut builder);

    let malformed = malformed_phi_reduction(&mut builder, sema);

    if malformed.is_error() {
        return Err(LowerError::MalformedPhiInfo(malformed));
    }

    cleanup_passes(&mut builder, pass_config);

    match builder.build(entry) {
        Ok(cfg) => Ok(cfg),
        Err(e) => Err(LowerError::CfgValidationErrors(e)),
    }
}

fn resolve_function_refs(builder: &mut CfgBuilder, sema: &SemaResults) {
    for block in &mut builder.blocks {
        for instr in &mut block.instructions {
            if let CfgInstruction::Assign { dest: _, val } = instr
                && let RValue::_VarId(v_id) = val
                && sema.var_types[v_id].is_function()
            {
                *val = RValue::Function { var_id: *v_id };
            }
        }
    }
}

fn lower_block_to_block(
    builder: &mut CfgBuilder,
    bb_id: &mut BBId,
    cf_loop: Option<ControlFlowLoopRefs>,
    block: &Spanned<Block>,
    sema: &SemaResults,
    semantic_errors: &mut Vec<SemanticError>,
    warnings: &mut Vec<CfgWarning>,
) {
    for stmt in &block.node.stmts {
        lower_stmt_to_block(
            builder,
            bb_id,
            cf_loop,
            stmt,
            sema,
            semantic_errors,
            warnings,
        );
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
        Stmt::Block(block) => {
            lower_block_to_block(
                builder,
                bb_id,
                cf_loop,
                block,
                sema,
                semantic_errors,
                warnings,
            );
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            // We lower if statments like this:
            //
            //                               current_bb
            //                                   |
            //                                  ... (evaluate cond)
            //                                   |
            //                  cond_br cond_val ? then_bb : else_bb
            //                                  /  \
            //                                 /    \
            //                                /      \
            //                               /        \
            //                           -------     -------
            //                           then_bb     else_bb
            //                              |           |
            //    (then statements)        ...         ...       (else statements, if any)
            //                               \         /
            //                                \       /
            //                                 \     /
            //                                  \   /
            //                                 -------
            //                                 exit_bb
            //                                    |
            //                             (continuation)
            let mut then_bb = builder.create_bb();

            let cond_val = lower_expr(builder, bb_id, cond, sema, semantic_errors);

            // Lower then branch
            lower_block_to_block(
                builder,
                &mut then_bb,
                cf_loop,
                then_branch,
                sema,
                semantic_errors,
                warnings,
            );
            // Lower else branch
            if let Some(else_branch) = else_branch {
                let mut else_bb = builder.create_bb();
                lower_block_to_block(
                    builder,
                    &mut else_bb,
                    cf_loop,
                    else_branch,
                    sema,
                    semantic_errors,
                    warnings,
                );
                let exit_bb = builder.create_bb();
                if !builder.bb_has_tail(else_bb) {
                    builder.set_tail_instruction(
                        else_bb,
                        TailCfgInstruction::UncondBranch { target: exit_bb },
                    );
                }

                builder.set_tail_instruction(
                    *bb_id,
                    TailCfgInstruction::CondBranch {
                        cond: cond_val,
                        then_bb,
                        else_bb,
                    },
                );

                if !builder.bb_has_tail(then_bb) {
                    builder.set_tail_instruction(
                        then_bb,
                        TailCfgInstruction::UncondBranch { target: exit_bb },
                    );
                }
                *bb_id = exit_bb;
            } else {
                let exit_bb = builder.create_bb();
                builder.set_tail_instruction(
                    *bb_id,
                    TailCfgInstruction::CondBranch {
                        cond: cond_val,
                        then_bb,
                        else_bb: exit_bb,
                    },
                );

                if !builder.bb_has_tail(then_bb) {
                    builder.set_tail_instruction(
                        then_bb,
                        TailCfgInstruction::UncondBranch { target: exit_bb },
                    );
                }
                *bb_id = exit_bb;
            }
        }
        Stmt::While { cond, body } => {
            // We lower while statements like this:
            //
            //                               current_bb
            //                                   |
            //                                  ...
            //                                   |
            //                                br entry_bb
            //                                   |
            //                               ----------
            //                                entry_bb <<------------------------------
            //                                   |                                    |
            //                                  ... (evaluate cond)                   |
            //                                   |                                    |
            //                          cond_br cond_val ? body_bb : exit_bb          |
            //                                 /  \                                   |
            //                                /    \                                  ^
            //                               /      \                                 |
            //                              /        \                                |
            //                          -------     -------                           |
            //                          exit_bb     body_bb                           |
            //                             |           |                              |
            //                      (continuation)    ... (body statements)           |
            //                                         |                              |
            //                                  br entry_bb -->------------------------

            let mut entry = builder.create_bb();
            let mut body_bb = builder.create_bb();

            builder
                .set_tail_instruction(*bb_id, TailCfgInstruction::UncondBranch { target: entry });

            let cond_val = lower_expr(builder, &mut entry, cond, sema, semantic_errors);

            let exit_bb = builder.create_bb();
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
            if !builder.bb_has_tail(body_bb) {
                builder.set_tail_instruction(
                    body_bb,
                    TailCfgInstruction::UncondBranch { target: entry },
                );
            }
            *bb_id = exit_bb;
        }
        Stmt::For {
            init,
            cond,
            update,
            body,
        } => {
            // We lower for statements like this:
            //
            //
            //                               current_bb
            //                                   |
            //                                  ...
            //                                   |
            //                                br entry_bb
            //                                   |
            //                               ----------
            //                                entry_bb <<------------------------------
            //                                   |                                    |
            //                                  ... (evaluate cond, if any)           |
            //                                   |                                    |
            //                          cond_br cond_val ? body_bb : exit_bb          |
            //                                 /  \                                   |
            //                                /    \                                  ^
            //                               /      \                                 |
            //                              /        \                                |
            //                          -------     -------                           |
            //                          exit_bb     body_bb                           |
            //                             |           |                              |
            //                      (continuation)    ... (body statements)           |
            //                                         |                              |
            //                                     ---------                          |
            //                                     update_bb                          |
            //                                         |                              |
            //                                        ... (update expr, if any)       |
            //                                         |                              |
            //                                  br entry_bb -->------------------------
            //
            // If the condition is ommitted, we do an unconditional branch to the body_bb. The only way to get out of a
            // for loop without a condition is via break statements, which we handle by setting the appropriate exit_bb as
            // the break target in the ControlFlowLoopRefs we pass for the body statements.

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
            if !builder.bb_has_tail(body_bb) {
                builder.set_tail_instruction(
                    body_bb,
                    TailCfgInstruction::UncondBranch { target: update_bb },
                );
            }
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

fn lower_bin_op<F>(
    builder: &mut CfgBuilder,
    bb: &mut BBId,
    root: &Spanned<Expr>,
    left: &Spanned<Expr>,
    right: &Spanned<Expr>,
    sema: &SemaResults,
    semantic_errors: &mut Vec<SemanticError>,
    op: F,
) -> ValueRef
where
    F: FnOnce(ValueRef, ValueRef) -> RValue,
{
    let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
    let right_val = lower_expr(builder, bb, right, sema, semantic_errors);
    let result_val = builder.allocate_value(root.span, None);
    builder.add_instruction(
        *bb,
        CfgInstruction::Assign {
            dest: result_val,
            val: op(left_val, right_val),
        },
    );
    result_val
}

fn lower_composite_assignment<F>(
    builder: &mut CfgBuilder,
    bb: &mut BBId,
    root: &Spanned<Expr>,
    left: &Spanned<Expr>,
    right: &Spanned<Expr>,
    sema: &SemaResults,
    semantic_errors: &mut Vec<SemanticError>,
    op: F,
) -> ValueRef
where
    F: FnOnce(ValueRef, ValueRef) -> RValue,
{
    let left_ident = if let Expr::Ident(id, _) = &**left {
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
    let left_val = builder.allocate_value(sema.var_span(left_var_id), Some(left_var_id));
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
            val: op(left_val, right_val),
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

            let val_ref = builder.allocate_value(root.span, Some(id));
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
            BinaryOp::Add => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::Add { left, right },
            ),
            BinaryOp::Subtract => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::Sub { left, right },
            ),
            BinaryOp::Multiply => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::Mul { left, right },
            ),
            BinaryOp::Divide => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::Div { left, right },
            ),
            BinaryOp::Modulus => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::Modulus { left, right },
            ),
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
                let left_val = builder.allocate_value(root.span, Some(var_id));
                builder.add_instruction(
                    *bb,
                    CfgInstruction::Assign {
                        dest: left_val,
                        val: RValue::Value(right_val),
                    },
                );
                builder.add_instruction(
                    *bb,
                    CfgInstruction::_AssignVar {
                        var_id,
                        val: RValue::Value(left_val),
                    },
                );
                left_val
            }
            BinaryOp::AddAssign => lower_composite_assignment(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::Add { left, right },
            ),
            BinaryOp::SubtractAssign => lower_composite_assignment(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::Sub { left, right },
            ),
            BinaryOp::MultiplyAssign => lower_composite_assignment(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::Mul { left, right },
            ),
            BinaryOp::DivideAssign => lower_composite_assignment(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::Div { left, right },
            ),
            BinaryOp::ModulusAssign => lower_composite_assignment(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::Modulus { left, right },
            ),
            BinaryOp::Equal => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::EqCheck { left, right },
            ),
            BinaryOp::NotEqual => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::NEqCheck { left, right },
            ),
            BinaryOp::LessThan => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::LtCheck { left, right },
            ),
            BinaryOp::GreaterThan => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::GtCheck { left, right },
            ),
            BinaryOp::LessThanOrEqual => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::LEqCheck { left, right },
            ),
            BinaryOp::GreaterThanOrEqual => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::GEqCheck { left, right },
            ),
            BinaryOp::And => {
                // We do short-circuit evaluation for logical AND
                //
                //                               current_bb
                //                                   |
                //                                  ... (evaluate left)
                //                                   |
                //                  cond_br left_val ? right_eval_bb : exit_bb
                //                                  /  \
                //                                 /    \
                //                                /      \
                //                           -------      \
                //                       right_eval_bb    |
                //                             |          |
                //           (evaluate right) ...         |
                //                             |          |
                //                       br exit_bb      /
                //                               \      /
                //                                \    /
                //                               -------
                //                               exit_bb
                //                                  |
                //            value = phi(right_eval_bb: right_val, current_bb: false)
                //                                  |
                //                            (continuation)

                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_eval_block_init = builder.create_bb();
                let mut right_eval_block = right_eval_block_init;
                // Lower right expression in its own block
                let right_val =
                    lower_expr(builder, &mut right_eval_block, right, sema, semantic_errors);
                let exit_bb = builder.create_bb();
                builder.set_tail_instruction(
                    *bb,
                    TailCfgInstruction::CondBranch {
                        cond: left_val,
                        then_bb: right_eval_block_init,
                        else_bb: exit_bb,
                    },
                );
                builder.set_tail_instruction(
                    right_eval_block,
                    TailCfgInstruction::UncondBranch { target: exit_bb },
                );
                // In exit block, create phi instruction to select correct value
                let result_val = builder.allocate_value(root.span, None);
                builder.add_phi_instruction(
                    exit_bb,
                    result_val,
                    [
                        (*bb, ValueRefOrConst::ConstBool(false)),
                        (right_eval_block, ValueRefOrConst::Value(right_val)), // right value
                    ]
                    .into_iter()
                    .collect(),
                    None,
                    PhiType::Bool,
                );
                *bb = exit_bb;

                result_val
            }
            BinaryOp::Or => {
                // We do short-circuit evaluation for logical OR
                //
                //                               current_bb
                //                                   |
                //                                  ... (evaluate left)
                //                                   |
                //                  cond_br left_val ? exit_bb : right_eval_bb
                //                                  /  \
                //                                 /    \
                //                                /      \
                //                           -------      \
                //                       right_eval_bb    |
                //                             |          |
                //           (evaluate right) ...         |
                //                             |          |
                //                       br exit_bb      /
                //                               \      /
                //                                \    /
                //                               -------
                //                               exit_bb
                //                                  |
                //            value = phi(right_eval_bb: right_val, current_bb: true)
                //                                  |
                //                            (continuation)
                let left_val = lower_expr(builder, bb, left, sema, semantic_errors);
                let right_eval_block_init = builder.create_bb();
                let mut right_eval_block = right_eval_block_init;
                // Lower right expression in its own block
                let right_val =
                    lower_expr(builder, &mut right_eval_block, right, sema, semantic_errors);
                let exit_bb = builder.create_bb();
                builder.set_tail_instruction(
                    *bb,
                    TailCfgInstruction::CondBranch {
                        cond: left_val,
                        then_bb: exit_bb,
                        else_bb: right_eval_block_init,
                    },
                );
                builder.set_tail_instruction(
                    right_eval_block,
                    TailCfgInstruction::UncondBranch { target: exit_bb },
                );
                // In exit block, create phi instruction to select correct value
                let result_val = builder.allocate_value(root.span, None);
                builder.add_phi_instruction(
                    exit_bb,
                    result_val,
                    [
                        (*bb, ValueRefOrConst::ConstBool(true)), // true if left was true
                        (right_eval_block, ValueRefOrConst::Value(right_val)), // right value
                    ]
                    .into_iter()
                    .collect(),
                    None,
                    PhiType::Bool,
                );
                *bb = exit_bb;

                result_val
            }
            BinaryOp::BitwiseAnd => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::BitwiseAnd { left, right },
            ),
            BinaryOp::BitwiseOr => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::BitwiseOr { left, right },
            ),
            BinaryOp::BitwiseXor => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::BitwiseXor { left, right },
            ),
            BinaryOp::LeftShift => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::BitShiftLeft { left, right },
            ),
            BinaryOp::RightShift => lower_bin_op(
                builder,
                bb,
                root,
                left,
                right,
                sema,
                semantic_errors,
                |left, right| RValue::BitShiftRight { left, right },
            ),
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
                PhiType::Int,
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

/// Insert phi nodes for all variables everywhere there is at least a value coming in from
/// any predecessor block.
///
/// If any of the phi nodes have sources that are missing, we handle that in the separate pass
/// malformed_phi_reduction below.
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
            let (v, _) = builder.add_phi_source(
                block,
                var_id,
                sources,
                sema.var_span(var_id),
                PhiType::Infer,
            );
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
                let (v, ch) = builder.add_phi_source(
                    block,
                    var_id,
                    sources,
                    sema.var_span(var_id),
                    PhiType::Infer,
                );
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
    pub phi_dest: ValueRef,
    pub phi_var_id: Option<VarId>,
    pub sources: BTreeMap<BBId, ValueRefOrConst>,
    pub missing_sources: BTreeSet<BBId>,
}

#[derive(Debug)]
pub struct SingleMalformedAssignmentInfo {
    pub bb_id: BBId,
    pub dest_val: ValueRef,
    pub assigned_val: RValue,
    pub missing_sources: BTreeSet<BBId>,
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
pub struct SingleMalformedTailInfo {
    pub bb_id: BBId,
    pub missing_sources: BTreeSet<BBId>,
}

#[derive(Debug)]
pub struct MalformedPhiInfo {
    pub infos: Vec<SingleMalformedPhiInfo>,
    pub malformed_assignments: Vec<SingleMalformedAssignmentInfo>,
    pub phi_assignments: Vec<SingleMalformedAssignmentInfo>,
    pub malformed_tails: Vec<SingleMalformedTailInfo>,
    pub blocks: Vec<BasicBlock>,
}

impl MalformedPhiInfo {
    fn is_error(&self) -> bool {
        self.malformed_assignments.len() > 0 || self.malformed_tails.len() > 0
    }

    pub fn display_in_graphviz(&self, sema: &SemaResults) -> String {
        let mut output = String::new();
        output.push_str("digraph MalformedPhiInfo {\n");
        output.push_str("  node [shape=box, fontname=\"Courier New\", fontsize=10];\n");
        output.push_str("  edge [fontname=\"Courier New\", fontsize=9];\n");

        let mut edges_missing_vars: BTreeMap<(BBId, BBId), BTreeSet<VarId>> = BTreeMap::new();

        for block in &self.blocks {
            let mut s = format!("BB{}:\n", block.id.0);

            for phi in &block.phi {
                let sources_str = phi
                    .sources
                    .iter()
                    .map(|(bb_id, val)| format!("{val}@BB{}", bb_id.0))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(&mut s, "    {} = Φ({}", phi.dest, sources_str).unwrap();
                self.infos
                    .iter()
                    .find(|info| info.bb_id == block.id && info.phi_dest == phi.dest)
                    .map(|info| {
                        if let Some(v) = info.phi_var_id {
                            for p in info.missing_sources.iter() {
                                edges_missing_vars
                                    .entry((*p, block.id))
                                    .or_default()
                                    .insert(v);
                            }
                        }
                        write!(
                            &mut s,
                            " [malformed: missing from BBs {:?}]",
                            info.missing_sources
                                .iter()
                                .filter(|it| block.predecessors.contains(it))
                                .map(|bb| bb.0)
                                .collect::<Vec<_>>()
                        )
                        .unwrap();
                    });
                write!(&mut s, ")").unwrap();

                if let Some(var_name) = phi.var_id.map(|it| sema.var_name(it)) {
                    write!(&mut s, " ({var_name})").unwrap();
                }
                writeln!(&mut s).unwrap();
            }

            for instr in &block.instructions {
                match instr {
                    CfgInstruction::Assign { dest, val } => {
                        write!(&mut s, "    {} = {}", dest, val).unwrap();
                        self.malformed_assignments
                            .iter()
                            .find(|info| info.bb_id == block.id && info.dest_val == *dest)
                            .or_else(|| {
                                self.phi_assignments
                                    .iter()
                                    .find(|info| info.bb_id == block.id && info.dest_val == *dest)
                            })
                            .map(|info| {
                                if let Some(v) = info.dest_val.2 {
                                    for p in info.missing_sources.iter() {
                                        edges_missing_vars
                                            .entry((*p, block.id))
                                            .or_default()
                                            .insert(v);
                                    }
                                }

                                write!(
                                    &mut s,
                                    " [malformed: missing from BBs {:?}]",
                                    info.missing_sources
                                        .iter()
                                        .filter(|it| block.predecessors.contains(it)
                                            || it == &&block.id)
                                        .map(|bb| bb.0)
                                        .collect::<Vec<_>>()
                                )
                                .unwrap();
                            });
                        if let Some(var_id) = dest.2 {
                            write!(&mut s, " ({})", sema.var_name(var_id)).unwrap();
                        }
                        writeln!(&mut s).unwrap();
                    }
                    CfgInstruction::_AssignVar { .. } => {
                        unreachable!()
                    }
                }
            }

            match &block.tail {
                TailCfgInstruction::UncondBranch { target } => {
                    writeln!(&mut s, "    br BB{}", target.0).unwrap();
                }
                TailCfgInstruction::CondBranch {
                    cond,
                    then_bb,
                    else_bb,
                } => {
                    writeln!(
                        &mut s,
                        "    br_cond {} ? BB{} : BB{};",
                        cond, then_bb.0, else_bb.0
                    )
                    .unwrap();
                }
                TailCfgInstruction::Return { value: Some(value) } => {
                    writeln!(&mut s, "    return {}", value).unwrap();
                }
                TailCfgInstruction::Return { value: None } => {
                    writeln!(&mut s, "    return").unwrap();
                }
                TailCfgInstruction::Undefined => {
                    writeln!(&mut s, "    <undefined>").unwrap();
                }
            }

            writeln!(&mut output, "  BB{} [label={s:?}];", block.id.0).unwrap();
        }

        fn get_missing_vars_label(
            edges_missing_vars: &BTreeMap<(BBId, BBId), BTreeSet<VarId>>,
            from: BBId,
            to: BBId,
            sema: &SemaResults,
        ) -> Option<String> {
            if let Some(vars) = edges_missing_vars.get(&(from, to)) {
                let var_names = vars
                    .iter()
                    .map(|v| sema.var_name(*v))
                    .collect::<Vec<_>>()
                    .join(", ");

                Some(format!(
                    "label=\"missing vars: {}\",fontcolor=red",
                    var_names
                ))
            } else {
                None
            }
        }

        for block in &self.blocks {
            match &block.tail {
                TailCfgInstruction::UncondBranch { target } => {
                    let missing_vars_extra =
                        get_missing_vars_label(&edges_missing_vars, block.id, *target, sema);
                    if let Some(extra) = missing_vars_extra {
                        writeln!(output, "  BB{} -> BB{} [{}];", block.id.0, target.0, extra)
                            .unwrap();
                    } else {
                        writeln!(output, "  BB{} -> BB{};", block.id.0, target.0).unwrap();
                    }
                }

                //Color true transitions green, false red
                TailCfgInstruction::CondBranch {
                    then_bb, else_bb, ..
                } => {
                    let missing_vars_then =
                        get_missing_vars_label(&edges_missing_vars, block.id, *then_bb, sema);
                    if let Some(extra) = missing_vars_then {
                        writeln!(
                            output,
                            "  BB{} -> BB{} [color=green,{}];",
                            block.id.0, then_bb.0, extra
                        )
                        .unwrap();
                    } else {
                        writeln!(
                            output,
                            "  BB{} -> BB{} [color=green];",
                            block.id.0, then_bb.0
                        )
                        .unwrap();
                    }
                    let missing_vars_else =
                        get_missing_vars_label(&edges_missing_vars, block.id, *else_bb, sema);
                    if let Some(extra) = missing_vars_else {
                        writeln!(
                            output,
                            "  BB{} -> BB{} [color=red,{}];",
                            block.id.0, else_bb.0, extra
                        )
                        .unwrap();
                    } else {
                        writeln!(output, "  BB{} -> BB{} [color=red];", block.id.0, else_bb.0)
                            .unwrap();
                    }
                }
                TailCfgInstruction::Return { .. } | TailCfgInstruction::Undefined => {}
            }
        }

        output.push_str("}\n");
        output
    }
}

/// This function removes any PHI instructions that have sources not matching the predecessors of the basic block
/// they are found in, as well as any assignments that assign from values that have been removed due to malformed phis.
///
/// In the end, after this pass, we should have a CFG with no malformed phis or assignments.
/// If any of the assignments removed come from the source code, then that means that some variable
/// is used before being initialized on all code paths, and we should report an error.
///
/// We track all this information in the returned [`MalformedPhiInfo`] struct, which can be used to
/// generate diagnostics later, as well as a graphviz representation of the malformed CFG, pointing
/// out the missing sources for each malformed phi/assignment, as well as the missing variables on edges.
fn malformed_phi_reduction(builder: &mut CfgBuilder, sema: &SemaResults) -> MalformedPhiInfo {
    let mut infos = Vec::new();
    let mut phi_assignments = Vec::new();
    let mut removed_vals = BTreeSet::new();
    let mut malformed_assignments = Vec::new();
    let mut phi_missing_sources = BTreeMap::<ValueRef, BTreeSet<BBId>>::new();
    let mut malformed_tails = Vec::new();
    let mut malformed_tail_blocks = BTreeSet::new();
    let blocks_clone = builder.blocks.clone();
    loop {
        let mut changed = false;
        for bb in &mut builder.blocks {
            let preds = bb.predecessors.iter().copied().collect::<BTreeSet<BBId>>();
            let malformed_phis = bb
                .phi
                .extract_if(0..bb.phi.len(), |phi| {
                    phi.sources.keys().copied().collect::<BTreeSet<BBId>>() != preds
                        || phi.sources.values().any(|s| match s {
                            ValueRefOrConst::Value(v) => removed_vals.contains(v),
                            _ => false,
                        })
                })
                .collect::<Vec<_>>();
            for phi in malformed_phis {
                removed_vals.insert(phi.dest);
                let missing_sources = preds
                    .difference(
                        &phi.sources
                            .iter()
                            .filter(|(_, v)| match v {
                                ValueRefOrConst::Value(v) => !removed_vals.contains(v),
                                _ => true,
                            })
                            .map(|(bb_id, _)| *bb_id)
                            .collect::<BTreeSet<BBId>>(),
                    )
                    .copied()
                    .collect::<BTreeSet<BBId>>();
                phi_missing_sources.insert(phi.dest, missing_sources.clone());
                infos.push(SingleMalformedPhiInfo {
                    bb_id: bb.id,
                    phi_dest: phi.dest,
                    phi_var_id: phi.var_id,
                    sources: phi.sources,
                    missing_sources,
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
                if let Some((dest, source_bb, v, _)) = CfgBuilder::is_optimized_phi(&instr) {
                    removed_vals.insert(dest);
                    // safe to propagate missing sources
                    let mut old = phi_missing_sources.get(&v).cloned().unwrap_or_default();
                    old.insert(source_bb);
                    phi_missing_sources.insert(dest, old.clone());

                    phi_assignments.push(SingleMalformedAssignmentInfo {
                        bb_id: bb.id,
                        dest_val: dest,
                        assigned_val: RValue::Value(v),
                        missing_sources: old,
                    });
                } else if let CfgInstruction::Assign { dest, val } = instr {
                    removed_vals.insert(dest);
                    // safe to propagate missing sources
                    let old = match val {
                        RValue::Value(v) => {
                            // phi_missing_sources.get(&v).cloned().unwrap_or_default()
                            BTreeSet::new()
                        }
                        RValue::_VarId(_) => {
                            // variable use = missing from all predecessor blocks
                            bb.predecessors.iter().copied().collect::<BTreeSet<BBId>>()
                        }
                        _ => unreachable!(),
                    };
                    // old.insert(bb.id);
                    phi_missing_sources.insert(dest, old.clone());

                    malformed_assignments.push(SingleMalformedAssignmentInfo {
                        missing_sources: old,
                        bb_id: bb.id,
                        dest_val: dest,
                        assigned_val: val,
                    });
                    changed = true;
                }
            }

            match &bb.tail {
                TailCfgInstruction::CondBranch {
                    cond: v,
                    then_bb: _,
                    else_bb: _,
                }
                | TailCfgInstruction::Return { value: Some(v) } => {
                    if removed_vals.contains(v) && !malformed_tail_blocks.contains(&bb.id) {
                        malformed_tails.push(SingleMalformedTailInfo {
                            bb_id: bb.id,
                            missing_sources: phi_missing_sources
                                .get(v)
                                .cloned()
                                .unwrap_or_default(),
                        });
                        malformed_tail_blocks.insert(bb.id);
                        bb.tail = TailCfgInstruction::Undefined;
                        changed = true;
                    }
                }
                _ => {}
            }
        }
        if !changed {
            break;
        }
    }

    MalformedPhiInfo {
        blocks: blocks_clone,
        infos,
        malformed_assignments,
        malformed_tails,
        phi_assignments,
    }
}
