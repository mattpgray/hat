use super::lexer::*;
use std::collections::HashMap;
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
pub struct Ast {
    pub procs: HashMap<String, Proc>,
}

#[derive(Debug)]
pub struct Proc {
    pub name: String,
    // TODO: Args are not accepted.
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    If {
        cond: Expr,
        then: Vec<Stmt>,
        else_: Option<Box<Stmt>>,
    },
    Block(Vec<Stmt>),
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
        let tok = l.peek();

        match tok.kind {
            TokenKind::OpenCurly => Ok(Stmt::Block(Self::parse_block(l)?)),
            TokenKind::If => Self::parse_if(l),
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
}

#[derive(Debug)]
pub enum Expr {
    IntLiteral(u64),
    IntrinsicCall(String, Vec<Expr>),
}

impl Expr {
    fn parse(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Self, SyntaxError> {
        let tok = l.next();
        match tok.kind {
            TokenKind::Word => {
                todo!(
                    "{}: Parsing of expressions that begin with a word is not implemented yet.",
                    tok.loc
                )
            }
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
            TokenKind::IntLiteral => Ok(Expr::IntLiteral(Self::parse_int_literal(tok.text))),
            TokenKind::EndOfFile
            | TokenKind::Invalid
            | TokenKind::OpenCurly
            | TokenKind::CloseCurly
            | TokenKind::OpenParen
            | TokenKind::CloseParen
            | TokenKind::SemiColon
            | TokenKind::Proc
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Comma => Err(SyntaxError::UnexpectedToken {
                loc: tok.loc,
                found: tok.kind,
                want: vec![TokenKind::Word, TokenKind::Hash, TokenKind::IntLiteral],
            }),
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

impl Ast {
    pub fn parse(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Self, SyntaxError> {
        let mut ast = Ast {
            procs: HashMap::new(),
        };
        loop {
            let tok = l.peek();
            match tok.kind {
                TokenKind::EndOfFile => break,
                _ => {
                    peek_expect_token_kind(l, TokenKind::Proc)?;
                    let proc = Self::parse_proc(l)?;
                    ast.procs.insert(proc.name.clone(), proc);
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
