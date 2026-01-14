use std::fmt;

use crate::parser::{operator::{BinaryOp, UnaryOp}, span::Span};

use super::span::Spanned;

#[cfg(feature = "wasm")]
use serde::Serialize;

#[cfg_attr(feature = "wasm", derive(Serialize))]
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Type {
    Int,
    Float,
    Char,
    Bool,
}

#[cfg_attr(feature = "wasm", derive(Serialize))]
#[cfg_attr(feature = "wasm", serde(transparent))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RefId(pub(super) usize);

#[cfg_attr(feature = "wasm", derive(Serialize))]
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expr {
    IntLit(i64),
    Ident(RefId, String),
    BinOp {
        op: Spanned<BinaryOp>,
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
    },
    UnaryOp {
        op: Spanned<UnaryOp>,
        expr: Box<Spanned<Expr>>,
        prefix: bool,
    },
    Ternary {
        cond: Box<Spanned<Expr>>,
        then_expr: Box<Spanned<Expr>>,
        else_expr: Box<Spanned<Expr>>,
    },
    Call {
        func: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
}

#[cfg_attr(feature = "wasm", derive(Serialize))]
#[cfg_attr(feature = "wasm", serde(transparent))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(pub(super) usize);

impl fmt::Display for VarId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[cfg_attr(feature = "wasm", derive(Serialize))]
#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
}

#[cfg_attr(feature = "wasm", derive(Serialize))]
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Stmt {
    Expr(Spanned<Expr>),
    Block(Spanned<Block>),
    If {
        cond: Spanned<Expr>,
        then_branch: Spanned<Block>,
        else_branch: Option<Spanned<Block>>,
    },
    While {
        cond: Spanned<Expr>,
        body: Box<Spanned<Block>>,
    },
    For {
        init: Option<Box<Spanned<Stmt>>>,
        cond: Option<Spanned<Expr>>,
        update: Option<Spanned<Expr>>,
        body: Box<Spanned<Block>>,
    },
    Return(Option<Spanned<Expr>>),
    Break,
    Continue,
    VarDecl {
        id: VarId,
        ty: Spanned<Type>,
        name: Spanned<String>,
        init: Option<Spanned<Expr>>,
    },
}

#[cfg_attr(feature = "wasm", derive(Serialize))]
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Function {
    pub return_type: Spanned<Type>,
    pub name: Spanned<String>,
    pub params: Vec<(Spanned<Type>, Spanned<String>, VarId)>,
    pub body: Option<Spanned<Block>>,
}

#[cfg_attr(feature = "wasm", derive(Serialize))]
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum TopLevel {
    Function(Function, VarId),
    GlobalVar {
        ty: Spanned<Type>,
        name: Spanned<String>,
        init: Option<Spanned<Expr>>,
        span: Span,
        id: VarId,
    },
}

#[cfg_attr(feature = "wasm", derive(Serialize))]
#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Spanned<TopLevel>>,
}
