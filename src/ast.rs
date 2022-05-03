use super::lexer::*;
use std::fmt;

#[derive(Debug)]
pub enum SyntaxError {
    InvalidToken(Loc, String),
    UnexpectedToken {
        loc: Loc,
        found: TokenKind,
        want: Vec<TokenKind>,
    },
    UnexectedEOF(Loc),
    UnmatchedBracket(Loc, TokenKind),
}

impl SyntaxError {
    pub fn loc(&self) -> &Loc {
        match self {
            SyntaxError::InvalidToken(loc, _)
            | SyntaxError::UnexpectedToken {
                loc,
                found: _,
                want: _,
            }
            | SyntaxError::UnexectedEOF(loc)
            | SyntaxError::UnmatchedBracket(loc, _) => loc,
        }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SyntaxError::InvalidToken(_, text) => write!(f, "invalid token {}", text),
            SyntaxError::UnexpectedToken {
                loc: _,
                found,
                want,
            } => {
                assert!(!want.is_empty());

                let mut s = String::new();
                s.push_str(&format!("{}", want[0]));
                if want.len() > 1 {
                    for w in want.iter().take(want.len() - 1).skip(1) {
                        s.push_str(&format!(", {}", w));
                    }
                    s.push_str(&format!(" or {}", want[want.len() - 1]));
                }
                write!(f, "unexpected token: {}, expected {}", found, s)
            }
            SyntaxError::UnexectedEOF(_) => {
                write!(f, "unexpected end of file")
            }
            SyntaxError::UnmatchedBracket(_, kind) => {
                write!(f, "unmatched bracket: {}", kind)
            }
        }
    }
}

#[derive(Debug)]
pub struct Var {
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub name: String,
    // TODO: Args are not accepted.
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    If {
        cond: Expr,
        then: Vec<Stmt>,
        else_: Option<Box<Stmt>>,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
    },
    Block(Vec<Stmt>),
    Assign(String, Expr),
}

impl Stmt {
    fn parse_block(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Vec<Stmt>, SyntaxError> {
        expect_token_kind(l, TokenKind::OpenCurly)?;
        let mut stmts = Vec::new();
        loop {
            let tok = l.peek();
            match tok.kind {
                TokenKind::CloseCurly => {
                    l.next();
                    break;
                }
                TokenKind::EndOfFile => {
                    return Err(SyntaxError::UnmatchedBracket(
                        tok.loc.clone(),
                        TokenKind::OpenCurly,
                    ))
                }
                _ => {
                    stmts.push(Self::parse(l)?);
                }
            }
        }
        Ok(stmts)
    }

    fn parse(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Stmt, SyntaxError> {
        let tok = l.peek().clone();

        match tok.kind {
            TokenKind::OpenCurly => Ok(Stmt::Block(Self::parse_block(l)?)),
            TokenKind::If => Self::parse_if(l),
            TokenKind::While => Self::parse_while(l),
            TokenKind::Word => {
                l.next();
                let p_tok = l.peek();
                if p_tok.kind == TokenKind::Eq {
                    l.next();
                    let assign = Stmt::Assign(tok.text, Expr::parse(l)?);
                    expect_token_kind(l, TokenKind::SemiColon)?;
                    Ok(assign)
                } else {
                    let expr = Expr::parse_word_expr(l, &tok)?;
                    expect_token_kind(l, TokenKind::SemiColon)?;
                    Ok(Stmt::Expr(expr))
                }
            }
            _ => {
                let expr = Expr::parse(l)?;
                expect_token_kind(l, TokenKind::SemiColon)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_if(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Stmt, SyntaxError> {
        expect_token_kind(l, TokenKind::If)?;
        let cond = Expr::parse(l)?;
        let then = Self::parse_block(l)?;

        let tok = l.peek();
        match tok.kind {
            TokenKind::Else => {
                l.next();
                let tok = l.peek();
                match tok.kind {
                    TokenKind::If => {
                        let else_ = Some(Box::new(Stmt::parse_if(l)?));
                        Ok(Stmt::If { cond, then, else_ })
                    }
                    _ => {
                        let block = Box::new(Stmt::Block(Self::parse_block(l)?));
                        let else_ = Some(block);
                        Ok(Stmt::If { cond, then, else_ })
                    }
                }
            }
            _ => Ok(Stmt::If {
                cond,
                then,
                else_: None,
            }),
        }
    }

    fn parse_while(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Stmt, SyntaxError> {
        expect_token_kind(l, TokenKind::While)?;
        let cond = Expr::parse(l)?;
        let body = Self::parse_block(l)?;
        Ok(Stmt::While { cond, body })
    }
}

#[derive(Debug, Clone)]
pub enum Op {
    Sub,
    Add,
    Mul,
    Div,
    Gt,
    Lt,
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntLiteral(u64),
    IntrinsicCall(String, Vec<Expr>),
    Word(String),
    Op {
        left: Box<Expr>,
        right: Box<Expr>,
        op: Op,
    },
}

impl Expr {
    fn parse(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Self, SyntaxError> {
        let expr = Self::parse_one(l)?;
        let tok = l.peek();
        match tok.kind {
            TokenKind::Minus => {
                l.next();
                Self::parse_op(l, expr, Op::Sub)
            }
            TokenKind::Add => {
                l.next();
                Self::parse_op(l, expr, Op::Add)
            }
            TokenKind::Mul => {
                l.next();
                Self::parse_op(l, expr, Op::Mul)
            }
            TokenKind::Div => {
                l.next();
                Self::parse_op(l, expr, Op::Div)
            }
            TokenKind::RAngleBracket => {
                l.next();
                Self::parse_op(l, expr, Op::Gt)
            }
            TokenKind::LAngleBracket => {
                l.next();
                Self::parse_op(l, expr, Op::Lt)
            }
            TokenKind::OpenCurly
            | TokenKind::CloseCurly
            | TokenKind::OpenParen
            | TokenKind::CloseParen
            | TokenKind::SemiColon
            | TokenKind::Hash
            | TokenKind::Comma
            | TokenKind::Eq
            | TokenKind::IntLiteral
            | TokenKind::Word
            | TokenKind::Proc
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Var
            | TokenKind::While
            | TokenKind::Invalid
            | TokenKind::EndOfFile => Ok(expr),
        }
    }

    fn parse_one(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Self, SyntaxError> {
        let tok = l.next();
        match tok.kind {
            TokenKind::Word => Self::parse_word_expr(l, &tok),
            TokenKind::Hash => {
                let name = expect_token_kind(l, TokenKind::Word)?.text;
                expect_token_kind(l, TokenKind::OpenParen)?;
                let mut args = Vec::new();
                loop {
                    let tok = l.peek();
                    match tok.kind {
                        TokenKind::CloseParen => {
                            l.next();
                            break;
                        }
                        _ => {
                            args.push(Self::parse(l)?);
                        }
                    }
                    let tok = l.peek();
                    match tok.kind {
                        TokenKind::Comma => {
                            l.next();
                        }
                        TokenKind::CloseParen => {
                            l.next();
                            break;
                        }
                        _ => {
                            return Err(SyntaxError::UnexpectedToken {
                                loc: tok.loc.clone(),
                                found: tok.kind.clone(),
                                want: vec![TokenKind::Comma, TokenKind::CloseParen],
                            })
                        }
                    }
                }
                Ok(Expr::IntrinsicCall(name, args))
            }
            TokenKind::Minus => {
                let expr = Expr::parse(l)?;
                Ok(Expr::Op {
                    left: Box::new(Expr::IntLiteral(0)),
                    right: Box::new(expr),
                    op: Op::Sub,
                })
            }
            TokenKind::IntLiteral => Ok(Expr::IntLiteral(Self::parse_int_literal(tok.text))),
            TokenKind::OpenParen => {
                let expr = Expr::parse(l)?;
                expect_token_kind(l, TokenKind::CloseParen)?;
                Ok(expr)
            }
            TokenKind::EndOfFile
            | TokenKind::Invalid
            | TokenKind::OpenCurly
            | TokenKind::CloseCurly
            | TokenKind::CloseParen
            | TokenKind::SemiColon
            | TokenKind::Proc
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Var
            | TokenKind::Eq
            | TokenKind::While
            | TokenKind::Add
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::LAngleBracket
            | TokenKind::RAngleBracket
            | TokenKind::Comma => Err(SyntaxError::UnexpectedToken {
                loc: tok.loc,
                found: tok.kind,
                want: vec![
                    TokenKind::Word,
                    TokenKind::Minus,
                    TokenKind::Hash,
                    TokenKind::IntLiteral,
                ],
            }),
        }
    }

    fn parse_op(
        l: &mut Lexer<impl Iterator<Item = char>>,
        left: Expr,
        op: Op,
    ) -> Result<Expr, SyntaxError> {
        let right = Expr::parse(l)?;
        Ok(Expr::Op {
            left: Box::new(left),
            right: Box::new(right),
            op,
        })
    }

    fn parse_word_expr(
        _l: &mut Lexer<impl Iterator<Item = char>>,
        word_tok: &Token,
    ) -> Result<Self, SyntaxError> {
        assert!(word_tok.kind == TokenKind::Word, "Expected word token");
        let tok = _l.peek();
        match tok.kind {
            TokenKind::OpenParen => {
                todo!(
                    "{}: Parsing of function calls is not implented yet",
                    word_tok.loc
                )
            }
            _ => Ok(Expr::Word(word_tok.text.clone())),
        }
    }

    fn parse_int_literal(text: String) -> u64 {
        let mut chars = text.chars();
        let mut int_val = chars.next().unwrap() as u64 - '0' as u64;
        for c in chars {
            int_val = int_val * 10 + (c as u64 - '0' as u64);
        }
        int_val
    }
}

#[derive(Debug)]
pub enum Decl {
    Var(Var),
    Proc(Proc),
}

#[derive(Debug)]
pub struct Ast {
    pub decls: Vec<Decl>,
}

impl Ast {
    pub fn parse(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Self, SyntaxError> {
        let mut ast = Ast { decls: Vec::new() };
        loop {
            let tok = l.peek();
            match tok.kind {
                TokenKind::EndOfFile => break,
                TokenKind::Proc => {
                    let proc = Self::parse_proc(l)?;
                    ast.decls.push(Decl::Proc(proc));
                }
                TokenKind::Var => {
                    let var = Self::parse_var(l)?;
                    ast.decls.push(Decl::Var(var));
                }
                _ => {
                    return Err(SyntaxError::UnexpectedToken {
                        loc: tok.loc.clone(),
                        found: tok.kind.clone(),
                        want: vec![TokenKind::Proc, TokenKind::Var, TokenKind::EndOfFile],
                    })
                }
            }
        }
        Ok(ast)
    }

    fn parse_proc(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Proc, SyntaxError> {
        expect_token_kind(l, TokenKind::Proc)?;
        let name = expect_token_kind(l, TokenKind::Word)?.text;
        expect_token_kind(l, TokenKind::OpenParen)?;
        // TODO: Parse args.
        expect_token_kind(l, TokenKind::CloseParen)?;

        // TODO: Parse return types

        peek_expect_token_kind(l, TokenKind::OpenCurly)?;

        Ok(Proc {
            name,
            body: Stmt::parse_block(l)?,
        })
    }

    fn parse_var(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Var, SyntaxError> {
        expect_token_kind(l, TokenKind::Var)?;
        let name = expect_token_kind(l, TokenKind::Word)?.text;

        let tok = l.next();
        match tok.kind {
            TokenKind::Eq => {
                // TODO: Allow arbitrary expressions.
                let expr = Expr::parse(l)?;
                expect_token_kind(l, TokenKind::SemiColon)?;
                Ok(Var {
                    name,
                    value: Some(expr),
                })
            }
            TokenKind::SemiColon => Ok(Var { name, value: None }),
            _ => Err(SyntaxError::UnexpectedToken {
                loc: tok.loc,
                found: tok.kind,
                want: vec![TokenKind::Eq, TokenKind::SemiColon],
            }),
        }
    }
}

fn expect_token_kind(
    l: &mut Lexer<impl Iterator<Item = char>>,
    kind: TokenKind,
) -> Result<Token, SyntaxError> {
    let tok = l.next();
    token_expected_kind(&tok, kind)?;
    Ok(tok)
}

fn peek_expect_token_kind(
    l: &mut Lexer<impl Iterator<Item = char>>,
    kind: TokenKind,
) -> Result<&Token, SyntaxError> {
    let tok = l.peek();
    token_expected_kind(tok, kind)?;
    Ok(tok)
}

fn token_expected_kind(tok: &Token, kind: TokenKind) -> Result<(), SyntaxError> {
    match tok.kind.clone() {
        TokenKind::EndOfFile => Err(SyntaxError::UnexectedEOF(tok.loc.clone())),
        TokenKind::Invalid => Err(SyntaxError::InvalidToken(tok.loc.clone(), tok.text.clone())),
        _ => {
            if kind != tok.kind {
                Err(SyntaxError::UnexpectedToken {
                    loc: tok.loc.clone(),
                    found: tok.kind.clone(),
                    want: vec![kind],
                })
            } else {
                Ok(())
            }
        }
    }
}
