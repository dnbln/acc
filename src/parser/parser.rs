use crate::parser::ast::{Program, RefId, VarId};
use crate::parser::span::Span;

use super::ast::{Expr, Function, Stmt, TopLevel, Type};
use super::error::{ParseError, ParseErrorKind, ParseResult};
use super::operator::{OpConfig, c_operators};
use super::span::Spanned;
use super::token::{Token, TokenKind};
use logos::Logos;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    config: OpConfig,
    var_counter: VarId,
    ref_counter: RefId,
}

impl Parser {
    pub fn new(input: impl AsRef<str>) -> Result<Self, Vec<ParseError>> {
        let config = c_operators();
        let source_len = input.as_ref().len();
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        for (result, span) in TokenKind::lexer(input.as_ref()).spanned() {
            match result {
                Ok(kind) => tokens.push(Token {
                    kind,
                    span: Span {
                        start: span.start,
                        end: span.end,
                    },
                }),
                Err(_) => errors.push(ParseError::new(
                    ParseErrorKind::InvalidToken,
                    Span {
                        start: span.start,
                        end: span.end,
                    },
                )),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span {
                start: source_len,
                end: source_len,
            },
        });

        Ok(Parser {
            tokens,
            pos: 0,
            config,
            var_counter: VarId(0),
            ref_counter: RefId(0),
        })
    }

    fn allocate_var_id(&mut self) -> VarId {
        let var_id = self.var_counter;
        self.var_counter.0 += 1;
        var_id
    }

    fn allocate_ref_id(&mut self) -> RefId {
        let ref_id = self.ref_counter;
        self.ref_counter.0 += 1;
        ref_id
    }

    // Utility methods to interact with tokens
    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn peek_kind(&self) -> &TokenKind {
        &self.tokens[self.pos].kind
    }

    fn current_span(&self) -> Span {
        self.tokens[self.pos].span
    }

    fn advance(&mut self) -> Token {
        let tok = self.tokens[self.pos].clone();
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
        tok
    }

    fn expect(&mut self, expected: TokenKind) -> ParseResult<Token> {
        let tok = self.peek().clone();
        if std::mem::discriminant(&tok.kind) == std::mem::discriminant(&expected) {
            Ok(self.advance())
        } else if matches!(tok.kind, TokenKind::Eof) {
            Err(ParseError::new(
                ParseErrorKind::UnexpectedEof {
                    expected: expected.description().to_string(),
                },
                tok.span,
            ))
        } else {
            Err(ParseError::new(
                ParseErrorKind::UnexpectedToken {
                    expected: expected.description().to_string(),
                    found: tok.kind.description().to_string(),
                },
                tok.span,
            ))
        }
    }

    fn next_is(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(self.peek_kind()) == std::mem::discriminant(kind)
    }

    fn span_from(&self, start: usize) -> Span {
        let end = if self.pos > 0 {
            self.tokens[self.pos - 1].span.end
        } else {
            start
        };
        Span { start, end }
    }

    fn parse_type(&mut self) -> ParseResult<Spanned<Type>> {
        let tok = self.peek().clone();
        let ty = match &tok.kind {
            TokenKind::Int => {
                self.advance();
                Type::Int
            }
            TokenKind::Float => {
                self.advance();
                Type::Float
            }
            TokenKind::Char => {
                self.advance();
                Type::Char
            }
            _ => {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedType {
                        found: tok.kind.description().to_string(),
                    },
                    tok.span,
                ));
            }
        };
        Ok(Spanned::new(ty, tok.span))
    }

    fn parse_typed_ident(&mut self) -> ParseResult<(Spanned<Type>, Spanned<String>)> {
        let ty = self.parse_type()?;

        let name_tok = self.peek().clone();
        let name = match &name_tok.kind {
            TokenKind::Ident(n) => {
                self.advance();
                Spanned::new(n.clone(), name_tok.span)
            }
            _ => {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedIdentifier {
                        found: name_tok.kind.description().to_string(),
                    },
                    name_tok.span,
                ));
            }
        };

        Ok((ty, name))
    }

    fn parse_typed_ident_with_var_id(
        &mut self,
    ) -> ParseResult<(Spanned<Type>, Spanned<String>, VarId)> {
        let ty = self.parse_type()?;

        let name_tok = self.peek().clone();
        let name = match &name_tok.kind {
            TokenKind::Ident(n) => {
                self.advance();
                Spanned::new(n.clone(), name_tok.span)
            }
            _ => {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedIdentifier {
                        found: name_tok.kind.description().to_string(),
                    },
                    name_tok.span,
                ));
            }
        };

        Ok((ty, name, self.allocate_var_id()))
    }

    // Cool blog post:
    // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    pub fn parse_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> ParseResult<Spanned<Expr>> {
        let start = self.current_span().start;
        let mut lhs = self.parse_prefix()?;

        loop {
            // Function calls
            if self.next_is(&TokenKind::LParen) {
                let lparen = self.advance();
                let args = self.parse_args(lparen.span.clone())?;
                self.expect(TokenKind::RParen)?;
                let span = self.span_from(start);
                lhs = Spanned::new(
                    Expr::Call {
                        func: Box::new(lhs),
                        args,
                    },
                    span,
                );
                continue;
            }

            // Postfix operators
            {
                let kind = self.peek_kind();
                if let Some(op_str) = kind.as_op_str() {
                    if let Some(bp) = self.config.get_postfix(op_str) {
                        if bp >= min_bp {
                            let op = kind.as_unary_op().unwrap();
                            let op_span = self.current_span();
                            self.advance();
                            let span = self.span_from(start);
                            lhs = Spanned::new(
                                Expr::UnaryOp {
                                    op: Spanned::new(op, op_span),
                                    expr: Box::new(lhs),
                                    prefix: false,
                                },
                                span,
                            );
                            continue;
                        }
                    }
                }
            }

            // Infix operators
            let kind = self.peek_kind();
            let (op, l_bp, r_bp) = match kind.as_op_str() {
                Some(op_str) => match self.config.get_infix(op_str) {
                    Some((l, r)) => (kind.as_binary_op().unwrap(), l, r),
                    None => break,
                },
                None => break,
            };

            if l_bp < min_bp {
                break;
            }
            let op_span = self.current_span();

            self.advance();

            let rhs = self.parse_expr_bp(r_bp)?;
            let span = self.span_from(start);
            lhs = Spanned::new(
                Expr::BinOp {
                    op: Spanned::new(op, op_span),
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                },
                span,
            );
        }

        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> ParseResult<Spanned<Expr>> {
        let tok = self.peek().clone();
        let start = tok.span.start;

        match &tok.kind {
            TokenKind::IntLit(n) => {
                let n = *n;
                self.advance();
                Ok(Spanned::new(Expr::IntLit(n), tok.span))
            }
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                let ref_id = self.ref_counter;
                self.ref_counter.0 += 1;
                Ok(Spanned::new(Expr::Ident(ref_id, name), tok.span))
            }
            TokenKind::LParen => {
                let lparen = self.advance();

                let expr = self.parse_expr()?;
                if self.next_is(&TokenKind::Eof) {
                    return Err(ParseError::new(
                        ParseErrorKind::UnclosedDelimiter {
                            delimiter: "parenthesis '('",
                            opened_at: lparen.span,
                        },
                        self.current_span(),
                    ));
                }
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            _ => {
                if let Some(op_str) = tok.kind.as_op_str() {
                    if let Some(bp) = self.config.get_prefix(op_str) {
                        let op = tok.kind.as_unary_op().unwrap();
                        self.advance();
                        let expr = self.parse_expr_bp(bp)?;
                        let span = self.span_from(start);
                        return Ok(Spanned::new(
                            Expr::UnaryOp {
                                op: Spanned::new(op, tok.span),
                                expr: Box::new(expr),
                                prefix: true,
                            },
                            span,
                        ));
                    } else {
                        return Err(ParseError::new(
                            ParseErrorKind::UnknownPrefixOperator(op_str.to_string()),
                            tok.span,
                        ));
                    }
                }
                Err(ParseError::new(
                    ParseErrorKind::ExpectedExpression {
                        found: tok.kind.description().to_string(),
                    },
                    tok.span,
                ))
            }
        }
    }

    fn is_type_start(&self) -> bool {
        matches!(
            self.peek_kind(),
            TokenKind::Int | TokenKind::Float | TokenKind::Char
        )
    }

    fn parse_args(&mut self, lparen_span: Span) -> ParseResult<Vec<Spanned<Expr>>> {
        let mut args = Vec::new();

        if self.next_is(&TokenKind::RParen) {
            return Ok(args);
        }

        if self.next_is(&TokenKind::Eof) {
            return Err(ParseError::new(
                ParseErrorKind::UnclosedDelimiter {
                    delimiter: "parenthesis '('",
                    opened_at: lparen_span,
                },
                self.current_span(),
            ));
        }

        args.push(self.parse_expr()?);
        while self.next_is(&TokenKind::Comma) {
            self.advance();
            args.push(self.parse_expr()?);
        }

        Ok(args)
    }

    // Parsing different statements
    fn parse_stmt(&mut self) -> ParseResult<Spanned<Stmt>> {
        let start = self.current_span().start;

        let stmt = match self.peek_kind() {
            TokenKind::LBrace => return self.parse_block(),
            TokenKind::If => return self.parse_if(),
            TokenKind::While => return self.parse_while(),
            TokenKind::For => return self.parse_for(),
            TokenKind::Return => return self.parse_return(),
            TokenKind::Break => {
                self.advance();
                self.expect(TokenKind::Semi)?;
                Stmt::Break
            }
            TokenKind::Continue => {
                self.advance();
                self.expect(TokenKind::Semi)?;
                Stmt::Continue
            }
            _ if self.is_type_start() => return self.parse_var_decl(),
            _ => {
                let expr = self.parse_expr()?;
                self.expect(TokenKind::Semi)?;
                Stmt::Expr(expr)
            }
        };

        let span = self.span_from(start);
        Ok(Spanned::new(stmt, span))
    }

    fn parse_block(&mut self) -> ParseResult<Spanned<Stmt>> {
        let current = self.current_span();
        let lbrace = self.expect(TokenKind::LBrace)?;
        let mut stmts = Vec::new();

        while !self.next_is(&TokenKind::RBrace) && !self.next_is(&TokenKind::Eof) {
            stmts.push(self.parse_stmt()?);
        }

        if self.next_is(&TokenKind::Eof) {
            return Err(ParseError::new(
                ParseErrorKind::UnclosedDelimiter {
                    delimiter: "brace '{'",
                    opened_at: lbrace.span,
                },
                current,
            ));
        }

        self.expect(TokenKind::RBrace)?;
        let span = self.span_from(current.start);
        Ok(Spanned::new(Stmt::Block(stmts), span))
    }

    fn parse_if(&mut self) -> ParseResult<Spanned<Stmt>> {
        let current = self.current_span();
        self.expect(TokenKind::If)?;
        let lparen = self.expect(TokenKind::LParen)?;
        let cond = self.parse_expr()?;

        if self.next_is(&TokenKind::Eof) {
            return Err(ParseError::new(
                ParseErrorKind::UnclosedDelimiter {
                    delimiter: "parenthesis '('",
                    opened_at: lparen.span,
                },
                current,
            ));
        }
        self.expect(TokenKind::RParen)?;

        let then_branch = Box::new(self.parse_stmt()?);

        let else_branch = if self.next_is(&TokenKind::Else) {
            self.advance();
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };

        let span = self.span_from(current.start);
        Ok(Spanned::new(
            Stmt::If {
                cond,
                then_branch,
                else_branch,
            },
            span,
        ))
    }

    fn parse_while(&mut self) -> ParseResult<Spanned<Stmt>> {
        let start = self.current_span().start;
        self.expect(TokenKind::While)?;
        let lparen = self.expect(TokenKind::LParen)?;
        let cond = self.parse_expr()?;

        if self.next_is(&TokenKind::Eof) {
            return Err(ParseError::new(
                ParseErrorKind::UnclosedDelimiter {
                    delimiter: "parenthesis '('",
                    opened_at: lparen.span,
                },
                self.current_span(),
            ));
        }
        self.expect(TokenKind::RParen)?;

        let body = Box::new(self.parse_stmt()?);

        let span = self.span_from(start);
        Ok(Spanned::new(Stmt::While { cond, body }, span))
    }

    fn parse_for(&mut self) -> ParseResult<Spanned<Stmt>> {
        let start = self.current_span().start;
        self.expect(TokenKind::For)?;
        let lparen = self.expect(TokenKind::LParen)?;

        let init = if self.next_is(&TokenKind::Semi) {
            self.advance();
            None
        } else if self.is_type_start() {
            Some(Box::new(self.parse_var_decl()?))
        } else {
            let expr = self.parse_expr()?;
            self.expect(TokenKind::Semi)?;
            let span = expr.span.clone();
            Some(Box::new(Spanned::new(Stmt::Expr(expr), span)))
        };

        let cond = if self.next_is(&TokenKind::Semi) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.expect(TokenKind::Semi)?;

        let update = if self.next_is(&TokenKind::RParen) {
            None
        } else {
            Some(self.parse_expr()?)
        };

        if self.next_is(&TokenKind::Eof) {
            return Err(ParseError::new(
                ParseErrorKind::UnclosedDelimiter {
                    delimiter: "parenthesis '('",
                    opened_at: lparen.span,
                },
                self.current_span(),
            ));
        }
        self.expect(TokenKind::RParen)?;

        let body = Box::new(self.parse_stmt()?);

        let span = self.span_from(start);
        Ok(Spanned::new(
            Stmt::For {
                init,
                cond,
                update,
                body,
            },
            span,
        ))
    }

    fn parse_var_decl(&mut self) -> ParseResult<Spanned<Stmt>> {
        let start = self.current_span().start;
        let (ty, name) = self.parse_typed_ident()?;
        let init = if self.next_is(&TokenKind::Assign) {
            self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };
        self.expect(TokenKind::Semi)?;
        let span = self.span_from(start);
        Ok(Spanned::new(
            Stmt::VarDecl {
                id: self.allocate_var_id(),
                ty,
                name,
                init,
            },
            span,
        ))
    }

    // Function Parsing
    fn parse_fn(&mut self) -> ParseResult<Spanned<TopLevel>> {
        let start = self.current_span().start;

        let (ty, name) = self.parse_typed_ident()?;
        let lparen = self.expect(TokenKind::LParen)?;
        let params = self.parse_params(lparen.span.clone())?;

        if self.next_is(&TokenKind::Eof) {
            return Err(ParseError::new(
                ParseErrorKind::UnclosedDelimiter {
                    delimiter: "parenthesis '('",
                    opened_at: lparen.span,
                },
                self.current_span(),
            ));
        }
        self.expect(TokenKind::RParen)?;

        let body = Some(self.parse_block()?);

        let span = self.span_from(start);
        Ok(Spanned::new(
            TopLevel::Function(
                Function {
                    return_type: ty,
                    name,
                    params,
                    body,
                },
                self.allocate_var_id(),
            ),
            span,
        ))
    }

    fn parse_params(
        &mut self,
        lparen_span: Span,
    ) -> ParseResult<Vec<(Spanned<Type>, Spanned<String>, VarId)>> {
        let mut params = Vec::new();

        if self.next_is(&TokenKind::RParen) {
            return Ok(params);
        }

        if self.next_is(&TokenKind::Eof) {
            return Err(ParseError::new(
                ParseErrorKind::UnclosedDelimiter {
                    delimiter: "parenthesis '('",
                    opened_at: lparen_span,
                },
                self.current_span(),
            ));
        }

        params.push(self.parse_typed_ident_with_var_id()?);
        while self.next_is(&TokenKind::Comma) {
            self.advance();
            params.push(self.parse_typed_ident_with_var_id()?);
        }

        Ok(params)
    }

    fn parse_return(&mut self) -> ParseResult<Spanned<Stmt>> {
        let start = self.current_span().start;
        self.expect(TokenKind::Return)?;
        let value = if self.next_is(&TokenKind::Semi) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.expect(TokenKind::Semi)?;
        let span = self.span_from(start);
        Ok(Spanned::new(Stmt::Return(value), span))
    }

    // Main parsing loop
    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let mut items = Vec::new();

        while !self.next_is(&TokenKind::Eof) {
            items.push(self.parse_fn()?);
        }

        Ok(Program { items })
    }
}
