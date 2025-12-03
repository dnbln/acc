use super::span::Spanned;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Type {
    Int,
    Float,
    Char,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expr {
    IntLit(i64),
    Ident(String),
    BinOp {
        op: Spanned<String>,
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
    },
    UnaryOp {
        op: Spanned<String>,
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

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Stmt {
    Expr(Spanned<Expr>),
    Block(Vec<Spanned<Stmt>>),
    If {
        cond: Spanned<Expr>,
        then_branch: Box<Spanned<Stmt>>,
        else_branch: Option<Box<Spanned<Stmt>>>,
    },
    While {
        cond: Spanned<Expr>,
        body: Box<Spanned<Stmt>>,
    },
    For {
        init: Option<Box<Spanned<Stmt>>>,
        cond: Option<Spanned<Expr>>,
        update: Option<Spanned<Expr>>,
        body: Box<Spanned<Stmt>>,
    },
    Return(Option<Spanned<Expr>>),
    Break,
    Continue,
    VarDecl {
        ty: Spanned<Type>,
        name: Spanned<String>,
        init: Option<Spanned<Expr>>,
    },
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Function {
    pub return_type: Spanned<Type>,
    pub name: Spanned<String>,
    pub params: Vec<(Spanned<Type>, Spanned<String>)>,
    pub body: Option<Spanned<Stmt>>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum TopLevel {
    Function(Function),
    GlobalVar {
        ty: Spanned<Type>,
        name: Spanned<String>,
        init: Option<Spanned<Expr>>,
    },
}
