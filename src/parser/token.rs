use logos::{Logos, Span};

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
    #[token("void")]
    Void,

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
    #[regex(r#""([^"\\]|\\.)*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string()
    })]
    StringLit(String),

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
    #[token("->")]
    Arrow,
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
    #[token(".")]
    Dot,
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
            TokenKind::Shl => "<<",
            TokenKind::Shr => ">>",
            TokenKind::Dot => ".",
            TokenKind::Arrow => "->",
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
            TokenKind::Void => "'void'",
            TokenKind::FloatLit(_) => "float literal",
            TokenKind::IntLit(_) => "integer literal",
            TokenKind::CharLit(_) => "character literal",
            TokenKind::StringLit(_) => "string literal",
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
            TokenKind::Arrow => "'->'",
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
            TokenKind::Dot => "'.'",
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
