use logos::Logos;

use crate::parser::{operator::{BinaryOp, UnaryOp}, span::Span};

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(skip r"[ \t\n\r]+")]
#[logos(skip r"//[^\n]*")]
#[logos(skip r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/")]
pub enum TokenKind {
    // Keywords
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("return")]
    Return,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("int")]
    Int,
    #[token("float")]
    Float,
    #[token("char")]
    Char,
    #[token("bool")]
    Bool,

    // Literals
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().ok())]
    FloatLit(f64),
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    IntLit(i64),
    #[regex(r"'([^'\\]|\\.)'", |lex| {
        let s = lex.slice();
        s.chars().nth(1)
    })]
    CharLit(char),

    // Identifier
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    // Multi-char operators
    #[token("++")]
    PlusPlus,
    #[token("--")]
    MinusMinus,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("==")]
    Eq,
    #[token("!=")]
    Ne,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,
    #[token("+=")]
    PlusEq,
    #[token("-=")]
    MinusEq,
    #[token("*=")]
    MulEq,
    #[token("/=")]
    DivEq,
    #[token("%=")]
    ModEq,
    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,

    // Single-char operators and punctuation
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("=")]
    Assign,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("!")]
    Bang,
    #[token("&")]
    Amp,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,

    // Delimiters
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    Eof,
}

impl TokenKind {
    pub fn as_unary_op(&self) -> Option<UnaryOp> {
        Some(match self {
            TokenKind::Plus => UnaryOp::Plus,
            TokenKind::Minus => UnaryOp::Minus,
            TokenKind::Bang => UnaryOp::Not,
            TokenKind::Tilde => UnaryOp::BitwiseNot,
            TokenKind::PlusPlus => UnaryOp::Increment,
            TokenKind::MinusMinus => UnaryOp::Decrement,
            _ => return None,
        })
    }

    pub fn as_binary_op(&self) -> Option<BinaryOp> {
        Some(match self {
            TokenKind::Plus => BinaryOp::Add,
            TokenKind::Minus => BinaryOp::Subtract,
            TokenKind::Star => BinaryOp::Multiply,
            TokenKind::Slash => BinaryOp::Divide,
            TokenKind::Percent => BinaryOp::Modulus,
            TokenKind::Assign => BinaryOp::Assign,
            TokenKind::Eq => BinaryOp::Equal,
            TokenKind::Ne => BinaryOp::NotEqual,
            TokenKind::Lt => BinaryOp::LessThan,
            TokenKind::Gt => BinaryOp::GreaterThan,
            TokenKind::Le => BinaryOp::LessThanOrEqual,
            TokenKind::Ge => BinaryOp::GreaterThanOrEqual,
            TokenKind::And => BinaryOp::And,
            TokenKind::Or => BinaryOp::Or,
            TokenKind::Amp => BinaryOp::BitwiseAnd,
            TokenKind::Pipe => BinaryOp::BitwiseOr,
            TokenKind::Caret => BinaryOp::BitwiseXor,
            TokenKind::Shl => BinaryOp::LeftShift,
            TokenKind::Shr => BinaryOp::RightShift,
            TokenKind::PlusEq => BinaryOp::AddAssign,
            TokenKind::MinusEq => BinaryOp::SubtractAssign,
            TokenKind::MulEq => BinaryOp::MultiplyAssign,
            TokenKind::DivEq => BinaryOp::DivideAssign,
            TokenKind::ModEq => BinaryOp::ModulusAssign,
            _ => return None,
        })
    }

    pub fn as_op_str(&self) -> Option<&'static str> {
        Some(match self {
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Percent => "%",
            TokenKind::Assign => "=",
            TokenKind::Eq => "==",
            TokenKind::Ne => "!=",
            TokenKind::Lt => "<",
            TokenKind::Gt => ">",
            TokenKind::Le => "<=",
            TokenKind::Ge => ">=",
            TokenKind::And => "&&",
            TokenKind::Or => "||",
            TokenKind::Bang => "!",
            TokenKind::Amp => "&",
            TokenKind::Pipe => "|",
            TokenKind::Caret => "^",
            TokenKind::Tilde => "~",
            TokenKind::PlusPlus => "++",
            TokenKind::MinusMinus => "--",
            TokenKind::PlusEq => "+=",
            TokenKind::MinusEq => "-=",
            TokenKind::MulEq => "*=",
            TokenKind::DivEq => "/=",
            TokenKind::ModEq => "%=",
            TokenKind::Shl => "<<",
            TokenKind::Shr => ">>",
            _ => return None,
        })
    }

    pub fn description(&self) -> &'static str {
        match self {
            TokenKind::If => "'if'",
            TokenKind::Else => "'else'",
            TokenKind::While => "'while'",
            TokenKind::For => "'for'",
            TokenKind::Return => "'return'",
            TokenKind::Break => "'break'",
            TokenKind::Continue => "'continue'",
            TokenKind::Int => "'int'",
            TokenKind::Float => "'float'",
            TokenKind::Char => "'char'",
            TokenKind::Bool => "'bool'",
            TokenKind::FloatLit(_) => "float literal",
            TokenKind::IntLit(_) => "integer literal",
            TokenKind::CharLit(_) => "character literal",
            TokenKind::Ident(_) => "identifier",
            TokenKind::PlusPlus => "'++'",
            TokenKind::MinusMinus => "'--'",
            TokenKind::And => "'&&'",
            TokenKind::Or => "'||'",
            TokenKind::Eq => "'=='",
            TokenKind::Ne => "'!='",
            TokenKind::Le => "'<='",
            TokenKind::Ge => "'>='",
            TokenKind::PlusEq => "'+='",
            TokenKind::MinusEq => "'-='",
            TokenKind::MulEq => "'*='",
            TokenKind::DivEq => "'/='",
            TokenKind::ModEq => "'%='",
            TokenKind::Shl => "'<<'",
            TokenKind::Shr => "'>>'",
            TokenKind::Plus => "'+'",
            TokenKind::Minus => "'-'",
            TokenKind::Star => "'*'",
            TokenKind::Slash => "'/'",
            TokenKind::Percent => "'%'",
            TokenKind::Assign => "'='",
            TokenKind::Lt => "'<'",
            TokenKind::Gt => "'>'",
            TokenKind::Bang => "'!'",
            TokenKind::Amp => "'&'",
            TokenKind::Pipe => "'|'",
            TokenKind::Caret => "'^'",
            TokenKind::Tilde => "'~'",
            TokenKind::Comma => "','",
            TokenKind::Semi => "';'",
            TokenKind::LParen => "'('",
            TokenKind::RParen => "')'",
            TokenKind::LBrace => "'{'",
            TokenKind::RBrace => "'}'",
            TokenKind::Eof => "end of file",
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
