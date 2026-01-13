//! Semantic analysis.
//! Binds variable references to their corresponding declarations.
//!
//! Entry point is the [`sema`] function which takes an AST and produces
//! a [`SemaResults`] mapping references to declarations, or a list of [`SemaError`]s
//! if any semantic errors were found (such as undefined variables).

use std::collections::BTreeMap;

use crate::{
    diagnostics::IntoDiagnostic,
    parser::{
        TopLevel, Type,
        ast::{Block, Expr, Function, Program, RefId, Stmt, VarId},
        span::{Span, Spanned},
    },
};

pub enum VarType {
    Int,
    Float,
    Char,
    Bool,
    Function(Vec<Type>, Type),
}

impl VarType {
    pub fn from_ast_type(type_name: &Type) -> Self {
        match type_name {
            Type::Int => VarType::Int,
            Type::Float => VarType::Float,
            Type::Char => VarType::Char,
            Type::Bool => VarType::Bool,
        }
    }

    pub fn is_function(&self) -> bool {
        matches!(self, VarType::Function(_, _))
    }
}

pub struct SemaResults {
    pub m: BTreeMap<RefId, VarId>,
    pub refs: BTreeMap<RefId, Span>,
    pub vars: BTreeMap<VarId, Span>,
    pub var_names: BTreeMap<VarId, String>,
    pub var_types: BTreeMap<VarId, VarType>,
}

impl SemaResults {
    fn resolve(&mut self, ref_id: RefId, ref_span: Span, var_id: VarId) {
        self.m.insert(ref_id, var_id);
        self.refs.insert(ref_id, ref_span);
    }

    fn add_declaration(
        &mut self,
        var_id: VarId,
        var_span: Span,
        var_name: Option<String>,
        var_type: VarType,
    ) {
        self.vars.insert(var_id, var_span);
        if let Some(name) = var_name {
            self.var_names.insert(var_id, name);
        }
        self.var_types.insert(var_id, var_type);
    }

    pub fn lookup_ref_id(&self, ref_id: RefId) -> Option<VarId> {
        self.m.get(&ref_id).copied()
    }

    pub fn var_span(&self, var_id: VarId) -> Span {
        self.vars.get(&var_id).cloned().unwrap_or_default()
    }

    pub fn var_name(&self, var_id: VarId) -> String {
        self.var_names
            .get(&var_id)
            .cloned()
            .unwrap_or_else(|| var_id.to_string())
    }
}

struct SemaStackFrame {
    vars: BTreeMap<String, VarId>,
}

struct SemaStack {
    frames: Vec<SemaStackFrame>,
}

impl SemaStack {
    fn resolve(&self, name: &str) -> Option<VarId> {
        for frame in self.frames.iter().rev() {
            if let Some(var_id) = frame.vars.get(name) {
                return Some(*var_id);
            }
        }
        None
    }
}

pub enum SemaError {
    UndefinedVariable(Spanned<String>, RefId),
    RedeclaredVariable {
        new_span: Spanned<String>,
        old_span: Spanned<String>,
        new_id: VarId,
        old_id: VarId,
    },
}

impl IntoDiagnostic for SemaError {
    fn to_diagnostic(&self, file_id: usize) -> codespan_reporting::diagnostic::Diagnostic<usize> {
        match self {
            SemaError::UndefinedVariable(name_span, _ref_id) => {
                codespan_reporting::diagnostic::Diagnostic::error()
                    .with_message("Undefined variable")
                    .with_labels(vec![
                        codespan_reporting::diagnostic::Label::primary(
                            file_id,
                            name_span.span.range(),
                        )
                        .with_message(format!("The variable '{}' is not defined", name_span.node)),
                    ])
            }
            SemaError::RedeclaredVariable {
                new_span,
                old_span,
                new_id: _new_id,
                old_id: _old_id,
            } => codespan_reporting::diagnostic::Diagnostic::error()
                .with_message("Redeclared variable")
                .with_labels(vec![
                    codespan_reporting::diagnostic::Label::primary(file_id, new_span.span.range())
                        .with_message(format!(
                            "The variable '{}' is redeclared in the same scope",
                            new_span.node
                        )),
                    codespan_reporting::diagnostic::Label::secondary(
                        file_id,
                        old_span.span.range(),
                    )
                    .with_message(format!("Previous declaration of '{}' here", old_span.node)),
                ]),
        }
    }
}

pub fn sema(ast: &Program) -> Result<SemaResults, Vec<SemaError>> {
    let mut sema = SemaResults {
        m: BTreeMap::new(),
        refs: BTreeMap::new(),
        vars: BTreeMap::new(),
        var_names: BTreeMap::new(),
        var_types: BTreeMap::new(),
    };
    let mut sema_errors = Vec::new();

    let mut stack = SemaStack { frames: Vec::new() };
    {
        let mut top_level_names = SemaStackFrame {
            vars: BTreeMap::new(),
        };
        for item in ast.items.iter() {
            match &**item {
                TopLevel::GlobalVar {
                    name, id, span, ty, ..
                } => {
                    top_level_names.vars.insert(name.node.clone(), *id);
                    sema.add_declaration(
                        *id,
                        span.clone(),
                        Some(name.node.clone()),
                        VarType::from_ast_type(&ty.node),
                    );
                }
                TopLevel::Function(f, id) => {
                    top_level_names.vars.insert(f.name.node.clone(), *id);
                    let param_types: Vec<Type> =
                        f.params.iter().map(|(ty, _, _)| ty.node.clone()).collect();
                    let return_type = f.return_type.node.clone();
                    sema.add_declaration(
                        *id,
                        f.name.span,
                        Some(f.name.node.clone()),
                        VarType::Function(param_types, return_type),
                    );
                }
            }
        }
        stack.frames.push(top_level_names);
    }

    for item in ast.items.iter() {
        match &**item {
            TopLevel::Function(func, _) => {
                sema_function(func, &mut sema, &mut stack, &mut sema_errors)
            }
            TopLevel::GlobalVar { init, .. } => {
                // Global variables are already handled in the top-level scope
                if let Some(init) = init {
                    sema_expr(init, &mut sema, &mut stack, &mut sema_errors);
                }
            }
        }
    }

    assert_eq!(stack.frames.len(), 1);

    if sema_errors.is_empty() {
        Ok(sema)
    } else {
        Err(sema_errors)
    }
}

fn sema_function(
    ast: &Function,
    sema: &mut SemaResults,
    stack: &mut SemaStack,
    sema_errors: &mut Vec<SemaError>,
) {
    {
        let mut base_frame = SemaStackFrame {
            vars: BTreeMap::new(),
        };
        for (ty, name, var_id) in &ast.params {
            base_frame.vars.insert(name.node.clone(), *var_id);
            sema.add_declaration(
                *var_id,
                name.span,
                Some(name.node.clone()),
                VarType::from_ast_type(&ty.node),
            );
        }
        stack.frames.push(base_frame);
    }

    if let Some(body) = &ast.body {
        sema_block(body, sema, stack, sema_errors);
    }

    stack.frames.pop();
}

fn sema_block(
    block: &Spanned<Block>,
    sema: &mut SemaResults,
    stack: &mut SemaStack,
    sema_errors: &mut Vec<SemaError>,
) {
    stack.frames.push(SemaStackFrame {
        vars: BTreeMap::new(),
    });
    for stmt in &block.node.stmts {
        sema_stmt(stmt, sema, stack, sema_errors);
    }
    stack.frames.pop();
}

fn sema_stmt(
    stmt: &Spanned<Stmt>,
    sema: &mut SemaResults,
    stack: &mut SemaStack,
    sema_errors: &mut Vec<SemaError>,
) {
    match &**stmt {
        Stmt::Block(block) => {
            sema_block(block, sema, stack, sema_errors);
        }
        Stmt::VarDecl { id, name, init, ty } => {
            // first sema the initializer expression (if any), so that we cannot reference ourselves in the initializer
            if let Some(init_expr) = init {
                sema_expr(init_expr, sema, stack, sema_errors);
            }
            let prev = stack
                .frames
                .last_mut()
                .unwrap()
                .vars
                .insert(name.node.clone(), *id);

            if let Some(prev_id) = prev {
                sema_errors.push(SemaError::RedeclaredVariable {
                    new_span: name.clone(),
                    old_span: Spanned::new(name.node.clone(), sema.var_span(prev_id)),
                    new_id: *id,
                    old_id: prev_id,
                });
            }

            sema.add_declaration(
                *id,
                name.span,
                Some(name.node.clone()),
                VarType::from_ast_type(&ty.node),
            );
        }
        Stmt::Expr(spanned) => {
            sema_expr(spanned, sema, stack, sema_errors);
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            sema_expr(cond, sema, stack, sema_errors);
            sema_block(then_branch, sema, stack, sema_errors);
            if let Some(else_branch) = else_branch {
                sema_block(else_branch, sema, stack, sema_errors);
            }
        }
        Stmt::While { cond, body } => {
            sema_expr(cond, sema, stack, sema_errors);
            for stmt in body.node.stmts.iter() {
                sema_stmt(stmt, sema, stack, sema_errors);
            }
        }
        Stmt::For {
            init,
            cond,
            update,
            body,
        } => {
            // push new scope for for-loop (variables declared in init are scoped to the loop)
            stack.frames.push(SemaStackFrame {
                vars: BTreeMap::new(),
            });
            if let Some(init_stmt) = init {
                sema_stmt(init_stmt, sema, stack, sema_errors);
            }
            if let Some(cond_expr) = cond {
                sema_expr(cond_expr, sema, stack, sema_errors);
            }
            if let Some(update_expr) = update {
                sema_expr(update_expr, sema, stack, sema_errors);
            }
            for stmt in body.node.stmts.iter() {
                sema_stmt(stmt, sema, stack, sema_errors);
            }
            // pop for-loop scope
            stack.frames.pop();
        }
        Stmt::Return(spanned) => {
            if let Some(expr) = spanned {
                sema_expr(expr, sema, stack, sema_errors);
            }
        }
        Stmt::Break => {}
        Stmt::Continue => {}
    }
}

fn sema_expr(
    expr: &Spanned<Expr>,
    sema: &mut SemaResults,
    stack: &mut SemaStack,
    sema_errors: &mut Vec<SemaError>,
) {
    match &**expr {
        Expr::IntLit(_) => {
            // Nothing to do for integer literals
        }
        Expr::Ident(ref_id, name) => {
            if let Some(var_id) = stack.resolve(name) {
                sema.resolve(*ref_id, expr.span, var_id);
            } else {
                sema_errors.push(SemaError::UndefinedVariable(
                    Spanned::new(name.clone(), expr.span),
                    *ref_id,
                ));
            }
        }
        Expr::BinOp { op: _, left, right } => {
            sema_expr(left, sema, stack, sema_errors);
            sema_expr(right, sema, stack, sema_errors);
        }
        Expr::UnaryOp {
            op: _,
            expr,
            prefix: _,
        } => {
            sema_expr(expr, sema, stack, sema_errors);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            sema_expr(cond, sema, stack, sema_errors);
            sema_expr(then_expr, sema, stack, sema_errors);
            sema_expr(else_expr, sema, stack, sema_errors);
        }
        Expr::Call { func, args } => {
            sema_expr(func, sema, stack, sema_errors);
            for arg in args {
                sema_expr(arg, sema, stack, sema_errors);
            }
        }
    }
}
