use std::collections::HashMap;

use super::lexer::*;

#[derive(Debug)]
pub struct AST {
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
}

impl Stmt {
    fn parse_block(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Vec<Stmt>, Error> {
        expect_token_kind(l, TokenKind::OpenCurly)?;
        let mut stmts = Vec::new();
        loop {
            let tok = l.next();
            match tok.kind {
                TokenKind::CloseCurly => break,
                _ => {
                    stmts.push(Self::parse(l)?);
                }
            }
        }
        Ok(stmts)
    }

    fn parse(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Stmt, Error> {
        let tok = l.peek();

        let stmt = match tok.kind {
            TokenKind::OpenCurly => {
                todo!("Parsing of nested blocks is not implemented yet.");
            }
            _ => Ok(Stmt::Expr(Expr::parse(l)?)),
        }?;

        expect_token_kind(l, TokenKind::SemiColon)?;
        Ok(stmt)
    }
}

#[derive(Debug)]
pub enum Expr {
    IntLiteral(u64),
    IntrinsicCall(String, Vec<Expr>),
}

impl Expr {
    fn parse(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Self, Error> {
        let tok = l.next();
        match tok.kind {
            TokenKind::Word => {
                todo!("Parsing of expressions that begin with a word is not implemented yet.")
            }
            TokenKind::Hash => {
                expect_token_kind(l, TokenKind::Word)?;
                expect_token_kind(l, TokenKind::OpenParen)?;
                let mut args = Vec::new();
                loop {
                    let tok = l.next();
                    match tok.kind {
                        TokenKind::CloseParen => break,
                        _ => {
                            args.push(Self::parse(l)?);
                        }
                    }
                    match l.peek().kind {
                        TokenKind::Comma => {
                            l.next();
                        }
                        TokenKind::CloseParen => {
                            l.next();
                            break;
                        }
                        _ => return Err(Error::SyntaxError),
                    }
                }
                Ok(Expr::IntrinsicCall(tok.text, args))
            }
            TokenKind::IntLiteral(u) => Ok(Expr::IntLiteral(u)),
            TokenKind::EOF
            | TokenKind::Invalid
            | TokenKind::OpenCurly
            | TokenKind::CloseCurly
            | TokenKind::OpenParen
            | TokenKind::CloseParen
            | TokenKind::SemiColon
            | TokenKind::Keyword(_)
            | TokenKind::Comma => return Err(Error::SyntaxError),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    SyntaxError,
}

impl AST {
    pub fn parse(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Self, Error> {
        let mut ast = AST {
            procs: HashMap::new(),
        };
        loop {
            let tok = l.next();
            match tok.kind {
                TokenKind::EOF => break,
                TokenKind::Invalid => {
                    eprintln!("{}: invalid token: {}", tok.loc, tok.text);
                    return Err(Error::SyntaxError);
                }
                TokenKind::Keyword(Keyword::Proc) => {
                    let proc = Self::parse_func(l)?;
                    ast.procs.insert(proc.name.clone(), proc);
                }
                _ => {}
            }
        }
        Ok(ast)
    }

    fn parse_func(l: &mut Lexer<impl Iterator<Item = char>>) -> Result<Proc, Error> {
        let name = expect_token_kind(l, TokenKind::Word)?.text;
        expect_token_kind(l, TokenKind::OpenParen)?;
        // TODO: Parse args.
        expect_token_kind(l, TokenKind::CloseParen)?;
        // TODO: Parse return args
        expect_token_kind(l, TokenKind::CloseParen)?;

        let tok = l.peek();
        if tok.kind != TokenKind::OpenCurly {
            return Err(Error::SyntaxError);
        }

        Ok(Proc {
            name,
            body: Stmt::parse_block(l)?,
        })
    }
}

fn expect_token_kind(
    l: &mut Lexer<impl Iterator<Item = char>>,
    kind: TokenKind,
) -> Result<Token, Error> {
    let tok = l.next();
    if tok.kind != kind {
        return Err(Error::SyntaxError);
    }
    Ok(tok)
}
