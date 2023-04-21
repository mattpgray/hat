use core::fmt;
use std::path::{Path, PathBuf};

use super::lexer::*;

#[derive(Debug, Clone)]
pub struct Word {
    pub text: String,
    pub start: Loc,
}

impl Word {
    fn from(tok: Token) -> Self {
        //  assert!(
        //      tok.kind != TokenKind::Word,
        //      "Word should only be created for word tokens {tok:?}"
        //  );
        Word {
            text: tok.text,
            start: tok.loc,
        }
    }
}

// TODO: Refactor intrinsics to require the hash to be part of the word and then remove this
// struct and only use Word.

#[derive(Debug, Clone)]
pub struct IntrinsicName {
    pub hash_loc: Loc,
    pub name: Word,
}

impl IntrinsicName {
    fn from(hash_loc: Loc, tok: Token) -> Self {
        IntrinsicName {
            hash_loc,
            name: Word::from(tok),
        }
    }
}

// Node is a set of common methods for each node of the ast.
pub trait Node {
    fn start(&self) -> &Loc;
}

#[derive(Debug)]
pub enum ASTError {
    SyntaxError(SyntaxError),
    // An invalid file has been provided to the ast.
    InvalidFile(String, std::io::Error),
}

impl From<SyntaxError> for ASTError {
    fn from(err: SyntaxError) -> Self {
        ASTError::SyntaxError(err)
    }
}

#[derive(Debug)]
pub struct Var {
    pub start: Loc,
    pub name: Word,
    pub value: Option<Expr>,
    pub typ: Option<Word>,
}

impl Node for Var {
    fn start(&self) -> &Loc {
        &self.start
    }
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub start: Loc,
    pub name: Word,
    // TODO: Args are not accepted.
    pub body: Block,
    pub ret_types: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    While { start: Loc, cond: Expr, body: Block },
    Assign(Word, Expr),
}

impl Node for Stmt {
    fn start(&self) -> &Loc {
        match self {
            Stmt::While {
                start,
                cond: _,
                body: _,
            }
            | Stmt::Assign(Word { start, .. }, _) => start,
            Stmt::Expr(expr) => expr.start(),
        }
    }
}

impl Stmt {
    fn parse_while(l: &mut Lexer) -> Result<Stmt, SyntaxError> {
        let while_tok = expect_token_kind(l, TokenKind::While)?;
        let cond = Expr::parse(l)?;
        let body = Expr::parse_block(l)?;
        Ok(Stmt::While {
            start: while_tok.loc.clone(),
            cond,
            body,
        })
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

impl Op {
    fn precedence(&self) -> u8 {
        match self {
            Op::Gt | Op::Lt => 0,
            Op::Sub | Op::Add => 1,
            Op::Mul | Op::Div => 2,
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Op::Sub => write!(f, "-"),
            Op::Add => write!(f, "+"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Gt => write!(f, ">"),
            Op::Lt => write!(f, "<"),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Block {
    pub start: Loc,
    pub body: Vec<Stmt>,
    pub ret_expr: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    IntLiteral(Loc, u64),
    IntrinsicCall(IntrinsicName, Vec<Expr>),
    Word(Word),
    Op {
        left: Box<Expr>,
        right: Box<Expr>,
        op: Op,
    },
    // Need an explicit bracket expr to be able to differentiate (1+2) * 3 from 1 + 2 * 3.
    BracketExpr(Loc, Box<Expr>),
    If {
        start: Loc,
        cond: Box<Expr>,
        then: Box<Block>,
        else_: Option<Box<Expr>>,
    },
    // Blocks are a list of statements that can optionally end with an expression. If they
    // do end with an expression, it is returned from the block. e.g.
    // let a = {
    //     let b = example_func();
    //     let c = example_func_2(b);
    //     c + 3
    // }
    // This means that blocks are expressions themselves.
    Block(Box<Block>),
}

impl Node for Expr {
    fn start(&self) -> &Loc {
        match self {
            Expr::IntLiteral(start, _)
            | Expr::IntrinsicCall(
                IntrinsicName {
                    hash_loc: start, ..
                },
                _,
            )
            | Expr::Word(Word { start, .. })
            | Expr::BracketExpr(start, _)
            | Expr::If {
                start,
                cond: _,
                then: _,
                else_: _,
            } => start,
            Expr::Op {
                left,
                right: _,
                op: _,
            } => left.start(),
            Expr::Block(block) => &block.start,
        }
    }
}

impl Expr {
    // n_returns performs a quick search to determine how many values an expression returns. It
    // does not perform a strict lookup of all branches or return types of functions.
    fn has_return(&self) -> bool {
        // The function definition will need to be used here? Just assume some result.
        match self {
            Expr::IntLiteral(..)
            | Expr::IntrinsicCall(..)
            | Expr::Word(..)
            | Expr::Op { .. }
            | Expr::BracketExpr(..) => true,
            Expr::If {
                start: _,
                cond: _,
                then,
                else_: _,
            } => then.ret_expr.is_some(),
            Expr::Block(block) => block.ret_expr.is_some(),
        }
    }

    fn parse(l: &mut Lexer) -> Result<Self, SyntaxError> {
        let expr = Self::parse_one(l)?;
        let tok = l.peek()?;
        match tok.kind {
            TokenKind::Minus => {
                l.next()?;
                Self::parse_op(l, expr, Op::Sub)
            }
            TokenKind::Add => {
                l.next()?;
                Self::parse_op(l, expr, Op::Add)
            }
            TokenKind::Mul => {
                l.next()?;
                Self::parse_op(l, expr, Op::Mul)
            }
            TokenKind::Div => {
                l.next()?;
                Self::parse_op(l, expr, Op::Div)
            }
            TokenKind::RAngleBracket => {
                l.next()?;
                Self::parse_op(l, expr, Op::Gt)
            }
            TokenKind::LAngleBracket => {
                l.next()?;
                Self::parse_op(l, expr, Op::Lt)
            }
            TokenKind::OpenCurly
            | TokenKind::CloseCurly
            | TokenKind::OpenParen
            | TokenKind::CloseParen
            | TokenKind::SemiColon
            | TokenKind::Colon
            | TokenKind::Hash
            | TokenKind::Comma
            | TokenKind::Eq
            | TokenKind::IntLiteral
            | TokenKind::StringLiteral
            | TokenKind::Word
            | TokenKind::Proc
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::Var
            | TokenKind::While
            | TokenKind::Inject
            | TokenKind::EndOfFile => Ok(expr),
        }
    }

    fn parse_one(l: &mut Lexer) -> Result<Self, SyntaxError> {
        let tok = l.peek()?;
        match tok.kind {
            TokenKind::Word => {
                let tok = l.next()?;
                Self::parse_word_expr(l, &tok)
            }
            TokenKind::Hash => {
                let hash_loc = l.next()?.loc;
                let name_tok = expect_token_kind(l, TokenKind::Word)?;
                expect_token_kind(l, TokenKind::OpenParen)?;
                let mut args = Vec::new();
                loop {
                    let tok = l.peek()?;
                    match tok.kind {
                        TokenKind::CloseParen => {
                            l.next()?;
                            break;
                        }
                        _ => {
                            args.push(Self::parse(l)?);
                        }
                    }
                    let tok = l.peek()?;
                    match tok.kind {
                        TokenKind::Comma => {
                            l.next()?;
                        }
                        TokenKind::CloseParen => {
                            l.next()?;
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
                Ok(Expr::IntrinsicCall(
                    IntrinsicName::from(hash_loc, name_tok),
                    args,
                ))
            }
            TokenKind::Minus => {
                l.next()?;
                let expr = Expr::parse(l)?;
                Ok(Expr::Op {
                    // TODO: This should be an int literal with some kind of minus flag. We are
                    // having to use an invalid location as a hack.
                    left: Box::new(Expr::IntLiteral(
                        Loc {
                            row: 1,
                            col: 1,
                            file_path: "".to_string(),
                        },
                        0,
                    )),
                    right: Box::new(expr),
                    op: Op::Sub,
                })
            }
            TokenKind::IntLiteral => {
                let tok = l.next()?;
                Ok(Expr::IntLiteral(tok.loc, Self::parse_int_literal(tok.text)))
            }
            TokenKind::StringLiteral => {
                todo!("string literals in expressions are not supported yet")
            }
            TokenKind::OpenParen => {
                let tok = l.next()?;
                let expr = Expr::parse(l)?;
                expect_token_kind(l, TokenKind::CloseParen)?;
                Ok(Expr::BracketExpr(tok.loc, Box::new(expr)))
            }
            TokenKind::OpenCurly => Ok(Expr::Block(Box::new(Self::parse_block(l)?))),
            TokenKind::If => Self::parse_if(l),
            TokenKind::EndOfFile
            | TokenKind::CloseCurly
            | TokenKind::CloseParen
            | TokenKind::SemiColon
            | TokenKind::Colon
            | TokenKind::Proc
            | TokenKind::Else
            | TokenKind::Var
            | TokenKind::Eq
            | TokenKind::While
            | TokenKind::Add
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Inject
            | TokenKind::LAngleBracket
            | TokenKind::RAngleBracket
            | TokenKind::Comma => Err(unexpected_token(
                tok,
                vec![
                    TokenKind::Word,
                    TokenKind::Minus,
                    TokenKind::Hash,
                    TokenKind::IntLiteral,
                ],
            )),
        }
    }

    fn parse_op(l: &mut Lexer, left: Expr, op: Op) -> Result<Expr, SyntaxError> {
        let right = Expr::parse(l)?;
        if let Expr::Op { .. } = left {
            todo!("parse_op: case where left is an op is not supported yet. Can this even happen?");
        }

        if let Expr::Op {
            op: op_right,
            left: left2,
            right: right2,
        } = right.clone()
        {
            if op_right.precedence() >= op.precedence() {
                Ok(Expr::Op {
                    left: Box::new(left),
                    right: Box::new(right),
                    op,
                })
            } else {
                Ok(Expr::Op {
                    left: Box::new(Expr::Op {
                        left: Box::new(left),
                        right: left2,
                        op,
                    }),
                    right: right2,
                    op: op_right,
                })
            }
        } else {
            Ok(Expr::Op {
                left: Box::new(left),
                right: Box::new(right),
                op,
            })
        }
    }

    // TODO: Refactor me to take an owned token so that we do not need to clone the text and
    // location.
    fn parse_word_expr(_l: &mut Lexer, word_tok: &Token) -> Result<Self, SyntaxError> {
        assert!(word_tok.kind == TokenKind::Word, "Expected word token");
        let tok = _l.peek()?;
        match tok.kind {
            TokenKind::OpenParen => {
                todo!(
                    "{}: Parsing of function calls is not implemented yet",
                    word_tok.loc
                )
            }
            _ => Ok(Expr::Word(Word::from(word_tok.clone()))),
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

    fn parse_if(l: &mut Lexer) -> Result<Expr, SyntaxError> {
        let if_tok = expect_token_kind(l, TokenKind::If)?;
        let cond = Box::new(Expr::parse(l)?);
        let then = Box::new(Self::parse_block(l)?);

        let tok = l.peek()?;
        match tok.kind {
            TokenKind::Else => {
                l.next()?;
                let tok = l.peek()?;
                match tok.kind {
                    TokenKind::If => {
                        let else_ = Some(Box::new(Self::parse_if(l)?));
                        Ok(Self::If {
                            start: if_tok.loc,
                            cond,
                            then,
                            else_,
                        })
                    }
                    _ => {
                        // TODO: Maybe refactor to remove this double box.
                        let block = Self::parse_block(l)?;
                        let expr = Self::Block(Box::new(block));
                        let else_ = Some(Box::new(expr));
                        Ok(Self::If {
                            start: if_tok.loc,
                            cond,
                            then,
                            else_,
                        })
                    }
                }
            }
            _ => Ok(Self::If {
                start: if_tok.loc,
                cond,
                then,
                else_: None,
            }),
        }
    }

    fn parse_block(l: &mut Lexer) -> Result<Block, SyntaxError> {
        let start = expect_token_kind(l, TokenKind::OpenCurly)?.loc;
        let mut stmts = Vec::new();
        loop {
            let tok = l.peek()?;
            match tok.kind {
                TokenKind::CloseCurly => {
                    l.next()?;
                    break;
                }
                TokenKind::EndOfFile => {
                    return Err(SyntaxError::UnmatchedBracket(
                        tok.loc.clone(),
                        TokenKind::OpenCurly,
                    ))
                }
                _ => {
                    let stmt = Self::parse_stmt_unsafe(l)?;
                    match stmt.clone() {
                        // While does not end in a semicolon
                        Stmt::While { .. } => {
                            stmts.push(stmt);
                        }
                        // Assign must end in a semicolon
                        Stmt::Assign(..) => {
                            expect_token_kind(l, TokenKind::SemiColon)?;
                            stmts.push(stmt);
                        }
                        // An expression does not need to end in a semicolon. But, if it
                        // does not end with a semicolon, it must be the last expression
                        // in the block (and so end with '}').
                        Stmt::Expr(expr) => {
                            // If and block do not need to end with a semicolon
                            match expr {
                                Expr::If { .. } | Expr::Block(..) => {
                                    let tok = l.peek()?;
                                    if tok.kind == TokenKind::CloseCurly {
                                        l.next()?;
                                        // If the internal has a return expression, so do we.
                                        if expr.has_return() {
                                            return Ok(Block {
                                                start,
                                                body: stmts,
                                                ret_expr: Some(expr),
                                            });
                                        } else {
                                            stmts.push(Stmt::Expr(expr));
                                            return Ok(Block {
                                                start,
                                                body: stmts,
                                                ret_expr: None,
                                            });
                                        }
                                    } else {
                                        stmts.push(Stmt::Expr(expr.clone()));
                                    }
                                }
                                _ => {
                                    let tok = l.next()?;
                                    match tok.kind {
                                        TokenKind::SemiColon => {
                                            stmts.push(stmt);
                                        }
                                        TokenKind::CloseCurly => {
                                            return Ok(Block {
                                                start,
                                                body: stmts,
                                                ret_expr: Some(expr),
                                            });
                                        }
                                        _ => {
                                            return Err(unexpected_token(
                                                &tok,
                                                vec![TokenKind::SemiColon, TokenKind::CloseCurly],
                                            ))
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(Block {
            start,
            body: stmts,
            ret_expr: None,
        })
    }

    // Callers of this function will need to validate the semicolons.
    fn parse_stmt_unsafe(l: &mut Lexer) -> Result<Stmt, SyntaxError> {
        let tok = l.peek()?;

        match &tok.kind {
            TokenKind::While => Ok(Stmt::parse_while(l)?),
            TokenKind::Word => {
                let tok = l.next()?;
                let p_tok = l.peek()?;
                if p_tok.kind == TokenKind::Eq {
                    l.next()?;
                    let assign = Stmt::Assign(Word::from(tok), Expr::parse(l)?);
                    Ok(assign)
                } else {
                    let expr = Expr::parse_word_expr(l, &tok)?;
                    Ok(Stmt::Expr(expr))
                }
            }
            _ => Ok(Stmt::Expr(Expr::parse(l)?)),
        }
    }
}

#[derive(Debug)]
pub enum Decl {
    Var(Var),
    Proc(Proc),
}

impl Node for Decl {
    fn start(&self) -> &Loc {
        match self {
            Decl::Var(Var { start, .. }) | Decl::Proc(Proc { start, .. }) => start,
        }
    }
}

// Public methods
impl Decl {
    pub fn reference_loc(&self) -> &Loc {
        match self {
            Decl::Var(Var { name, .. }) | Decl::Proc(Proc { name, .. }) => &name.start,
        }
    }
}

// Internal methods.
impl Decl {
    fn parse_proc(l: &mut Lexer) -> Result<Proc, SyntaxError> {
        let start = expect_token_kind(l, TokenKind::Proc)?.loc;
        let name_tok = expect_token_kind(l, TokenKind::Word)?;
        expect_token_kind(l, TokenKind::OpenParen)?;
        // TODO: Parse args.
        expect_token_kind(l, TokenKind::CloseParen)?;

        // TODO: Parse return types

        peek_expect_token_kind(l, TokenKind::OpenCurly)?;

        Ok(Proc {
            start,
            name: Word::from(name_tok),
            body: Expr::parse_block(l)?,
            ret_types: vec![],
        })
    }

    fn parse_var(l: &mut Lexer) -> Result<Var, SyntaxError> {
        let start = expect_token_kind(l, TokenKind::Var)?.loc;
        let name_tok = expect_token_kind(l, TokenKind::Word)?;

        // Parse the variable type if we can.
        let tok = l.peek()?;
        let typ = match &tok.kind {
            TokenKind::Colon => {
                l.next()?;
                let tok = expect_token_kind(l, TokenKind::Word)?;
                Some(Word::from(tok))
            }
            // Handled below
            TokenKind::Eq | TokenKind::SemiColon => None,
            _ => {
                return Err(SyntaxError::UnexpectedToken {
                    loc: tok.loc.clone(),
                    found: tok.kind.clone(),
                    want: vec![TokenKind::Eq, TokenKind::SemiColon, TokenKind::Colon],
                })
            }
        };

        let tok = l.next()?;
        match &tok.kind {
            TokenKind::Eq => {
                let expr = Expr::parse(l)?;
                expect_token_kind(l, TokenKind::SemiColon)?;
                Ok(Var {
                    start,
                    name: Word::from(name_tok),
                    value: Some(expr),
                    typ,
                })
            }
            TokenKind::SemiColon => Ok(Var {
                start,
                name: Word::from(name_tok),
                value: None,
                typ,
            }),
            _ => Err(SyntaxError::UnexpectedToken {
                loc: tok.loc,
                found: tok.kind,
                want: vec![TokenKind::Eq, TokenKind::SemiColon],
            }),
        }
    }
}

#[derive(Debug)]
pub struct Ast {
    pub decls: Vec<Decl>,
}

impl Ast {
    pub fn from_file(file_path: String) -> Result<Self, ASTError> {
        let mut lexer = Lexer::from_file(file_path.clone())
            .map_err(|err| ASTError::InvalidFile(file_path.clone(), err))?;
        let mut decls = Vec::new();
        Self::append_decls(&mut lexer, &file_path, &mut decls)?;
        Ok(Ast { decls })
    }

    fn append_decls(
        l: &mut Lexer,
        current_file: &String,
        decls: &mut Vec<Decl>,
    ) -> Result<(), SyntaxError> {
        // First we parse the injections.
        loop {
            let tok = l.peek()?;
            if tok.kind != TokenKind::Inject {
                break;
            }
            l.next()?;
            let tok = expect_token_kind(l, TokenKind::StringLiteral)?;
            let injected_file = parse_string_literal(&tok)?;

            let injected_file = join_rel_paths(current_file, &injected_file);
            let mut injected_lexer = Lexer::from_file(injected_file.clone()).map_err(|err| {
                SyntaxError::InvalidInjection(tok.loc, injected_file.clone(), err)
            })?;

            Self::append_decls(&mut injected_lexer, &injected_file, decls)?;
            let _ = expect_token_kind(l, TokenKind::SemiColon)?;
        }

        // Then we parse the declarations.
        loop {
            let tok = l.peek()?;
            match tok.kind {
                TokenKind::EndOfFile => break,
                TokenKind::Proc => {
                    let proc = Decl::parse_proc(l)?;
                    decls.push(Decl::Proc(proc));
                }
                TokenKind::Var => {
                    let var = Decl::parse_var(l)?;
                    decls.push(Decl::Var(var));
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
        Ok(())
    }
}

fn parse_string_literal(quoted_tok: &Token) -> Result<String, SyntaxError> {
    let mut iterator = quoted_tok.text.chars();
    let first = iterator.next().expect("Should not be empty");
    assert!(first == '"', "Should start with \"");

    let mut s = String::new();
    while let Some(c) = iterator.next() {
        if c == '\\' {
            let c = iterator
                .next()
                .expect("The escape sequence should not be at the end of the string");
            match c {
                _ => {
                    todo!("escape sequence \\{c} is not supported yet");
                }
            }
        } else if c == '"' {
            assert!(
                iterator.next().is_none(),
                "The only unescaped \" should be at the end of the string"
            );
        } else {
            s.push(c)
        }
    }

    Ok(s)
}

// join two relative paths and return a new relative path
fn join_rel_paths(a: &String, b: &String) -> String {
    let mut a_path = PathBuf::from(a);
    a_path.pop(); // First pop off the file name
    let b_components = Path::new(b).components();
    for c in b_components {
        match c {
            // "."
            std::path::Component::CurDir => {} // nop
            // ".."
            std::path::Component::ParentDir => {
                if !a_path.pop() {
                    todo!("Better error for invalid path")
                }
            }
            // "part"
            std::path::Component::Normal(p) => {
                a_path = a_path.join(p);
            }
            std::path::Component::Prefix(_) | std::path::Component::RootDir => {
                todo!("Return better error for use of absolute paths")
            }
        }
    }
    // ew
    a_path.as_path().to_str().unwrap().to_owned()
}

fn expect_token_kind(l: &mut Lexer, kind: TokenKind) -> Result<Token, SyntaxError> {
    let tok = l.next()?;
    token_expected_kind(&tok, kind)?;
    Ok(tok)
}

fn peek_expect_token_kind(l: &mut Lexer, kind: TokenKind) -> Result<&Token, SyntaxError> {
    let tok = l.peek()?;
    token_expected_kind(tok, kind)?;
    Ok(tok)
}

fn token_expected_kind(tok: &Token, kind: TokenKind) -> Result<(), SyntaxError> {
    if kind == tok.kind {
        Ok(())
    } else {
        Err(unexpected_token(tok, vec![kind]))
    }
}

fn unexpected_token(tok: &Token, want: Vec<TokenKind>) -> SyntaxError {
    match tok.kind.clone() {
        TokenKind::EndOfFile => SyntaxError::UnexectedEOF(tok.loc.clone()),
        _ => SyntaxError::UnexpectedToken {
            loc: tok.loc.clone(),
            found: tok.kind.clone(),
            want,
        },
    }
}
