use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

use crate::{
    ast::{self, Node},
    lexer,
};

#[derive(Debug)]
pub enum Builtin {
    U64,
    Bool,
}

impl Builtin {
    pub fn from_str(str: &str) -> Option<Builtin> {
        match str {
            "u64" => Some(Builtin::U64),
            "bool" => Some(Builtin::Bool),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &str {
        match self {
            Builtin::U64 => "u64",
            Builtin::Bool => "bool",
        }
    }

    fn supports_op(&self, op: &ast::Op) -> bool {
        match self {
            Builtin::U64 => match op {
                ast::Op::Sub
                | ast::Op::Add
                | ast::Op::Mul
                | ast::Op::Div
                | ast::Op::Gt
                | ast::Op::Lt => true,
            },
            Builtin::Bool => match op {
                ast::Op::Sub
                | ast::Op::Add
                | ast::Op::Mul
                | ast::Op::Div
                | ast::Op::Gt
                | ast::Op::Lt => false,
            },
        }
    }
}

// TODO: Refactor me into type errors and other kinds of errors. For now, all errors are type
// errors except for no main function. Type errors should always have a location.
//
// TODO: Some of these should take words instead of locations and name.
#[derive(Debug)]
pub enum Error {
    InitializationCycle(lexer::Loc, String),
    UnresolvedReference(lexer::Loc, String),
    UnsupportedOp {
        loc: lexer::Loc,
        typ: String,
        op: ast::Op,
    },
    Mismatch {
        loc: lexer::Loc,
        left: String,
        right: String,
    },
    UntypedVariable(lexer::Loc, String),
    UnknownType(lexer::Loc, String),
    UnexpectedNumberOfTypes {
        loc: lexer::Loc,
        want: usize,
        got: usize,
    },
    DuplicateReference {
        curr: lexer::Loc,
        prev: lexer::Loc,
        word: String,
    },
    InvalidAssign {
        loc: lexer::Loc,
        name: String,
    },
    InvalidReturn {
        loc: lexer::Loc,
        want: Vec<String>,
        got: Vec<String>,
    },
    BranchMismatch {
        loc: lexer::Loc,
        first: Vec<String>,
        second: Vec<String>,
    },
    NoMainFunction,
}

#[derive(Debug)]
pub enum Decl<'a> {
    Var(Var<'a>),
    Proc(Proc<'a>),
}

impl Decl<'_> {
    fn reference_loc(&self) -> &lexer::Loc {
        match self {
            Decl::Var(Var {
                ast_var: ast::Var { name, .. },
                ..
            })
            | Decl::Proc(Proc {
                ast_proc: ast::Proc { name, .. },
                ..
            }) => &name.start,
        }
    }
}

#[derive(Debug)]
pub struct Var<'a> {
    pub ast_var: &'a ast::Var,
    // type_info contains information about the var that is discovered at type checking time. It
    // it in a ref cell so that we can mutate it more easily as we traverse the tree.
    pub type_info: RefCell<VarTypeInformation>,
}

#[derive(Debug)]
pub struct VarTypeInformation {
    pub state: CheckState,
    pub typ: String,
    pub global_proc_references: Vec<String>,
    pub global_var_references: Vec<String>,
}

impl Var<'_> {
    pub fn underlying_typ(&self) -> Builtin {
        Builtin::from_str(&self.type_info.borrow().typ).expect("Parsed var should have valid type")
    }
}

#[derive(Debug)]
pub struct Proc<'a> {
    pub ast_proc: &'a ast::Proc,
    pub type_info: RefCell<ProcTypeInfo<'a>>,
}

#[derive(Debug)]
pub struct ProcTypeInfo<'a> {
    pub state: CheckState,
    pub global_proc_references: Vec<String>,
    pub global_var_references: Vec<String>,
    pub scope: Option<Scope<'a>>,
}

#[derive(Debug)]
pub struct Scope<'a> {
    ast_block: &'a ast::Block,
    // local variable types will be here eventually.
    ret_types: Vec<String>,
    parent: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    fn new(ast_block: &'a ast::Block, parent: Option<&'a Scope<'a>>) -> Self {
        Self {
            ast_block,
            ret_types: vec![],
            parent,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum CheckState {
    Checked,
    InProgress,
    Unchecked,
}

pub struct Context<'a> {
    pub globals: HashMap<String, Decl<'a>>,
    pub ast: &'a ast::Ast,
}

impl<'a> Context<'a> {
    // If from does not return an error, the modes that use the type context can rely on the ast
    // being valid.
    pub fn from(ast: &'a ast::Ast) -> Result<Context<'a>, Error> {
        let mut context = Context {
            globals: HashMap::new(),
            ast,
        };
        for ast_decl in &ast.decls {
            let (name, decl) = match ast_decl {
                ast::Decl::Var(var) => (
                    &var.name,
                    Decl::Var(Var {
                        ast_var: var,
                        type_info: RefCell::new(VarTypeInformation {
                            state: CheckState::Unchecked,
                            typ: "".to_string(),
                            global_proc_references: vec![],
                            global_var_references: vec![],
                        }),
                    }),
                ),
                ast::Decl::Proc(proc) => (
                    &proc.name,
                    Decl::Proc(Proc {
                        ast_proc: proc,
                        type_info: RefCell::new(ProcTypeInfo {
                            state: CheckState::Unchecked,
                            global_proc_references: vec![],
                            global_var_references: vec![],
                            scope: None,
                        }),
                    }),
                ),
            };
            if let Some(prev) = context.globals.insert(name.text.clone(), decl) {
                return Err(Error::DuplicateReference {
                    curr: ast_decl.reference_loc().clone(),
                    prev: prev.reference_loc().clone(),
                    word: name.text.clone(),
                });
            }
        }

        let valid_main = match context.globals.get("main") {
            Some(decl) => {
                if let Decl::Proc(_) = decl {
                    true
                } else {
                    false
                }
            }
            None => false,
        };
        if !valid_main {
            return Err(Error::NoMainFunction);
        }

        for decl in &ast.decls {
            if let ast::Decl::Var(var) = decl {
                let var = context.get_var(&var.name.text);
                context.check_var(var)?;
            }
        }
        for decl in &ast.decls {
            if let ast::Decl::Proc(proc) = decl {
                let proc = context.get_proc(&proc.name.text);
                context.check_proc(proc)?;
            }
        }

        Ok(context)
    }

    // walk the variable declarations in initialization order. This should be safe to call as we
    // should have already checked for cycles.
    pub fn walk_var_declarations<F, E>(&self, f: &mut F) -> Result<(), E>
    where
        F: FnMut(&Var) -> Result<(), E>,
    {
        let mut visited = HashSet::new();
        for key in self.globals.keys() {
            self.walk_var_declarations_impl(key, &mut visited, f)?;
        }
        Ok(())
    }

    fn walk_var_declarations_impl<F, E>(
        &self,
        key: &String,
        visited: &mut HashSet<String>,
        f: &mut F,
    ) -> Result<(), E>
    where
        F: FnMut(&Var) -> Result<(), E>,
    {
        if visited.contains(key) {
            return Ok(());
        }

        let decl = self.globals.get(key).expect("key exists");
        match decl {
            Decl::Var(var) => {
                let references = &var.type_info.borrow().global_var_references;
                for reference in references {
                    self.walk_var_declarations_impl(reference, visited, f)?;
                }
            }
            _ => return Ok(()),
        }

        let var = self.get_var(key);
        f(var)?;
        visited.insert(key.to_owned());

        Ok(())
    }

    fn check_var(&self, var: &Var) -> Result<(), Error> {
        // First check if we are in a cycle, or if this has already been checked.
        {
            let type_info = var.type_info.borrow();
            if var.type_info.borrow().state == CheckState::Checked {
                return Ok(());
            }
            if type_info.state == CheckState::InProgress {
                return Err(Error::InitializationCycle(
                    var.ast_var.name.start.clone(),
                    var.ast_var.name.text.clone(),
                ));
            }
        }
        // Now mark as in in progress so that we can detect cycles.
        {
            let mut type_info = var.type_info.borrow_mut();
            type_info.state = CheckState::InProgress;
        }

        let mut var_references = HashSet::new();
        let mut proc_references = HashSet::new();
        let ret_typ = {
            // If the type is declared then it must be valid.
            if let Some(typ) = &var.ast_var.typ {
                self.check_type(typ)?;
            }
            match &var.ast_var.value {
                Some(expr) => {
                    let ret_types =
                        self.check_expr(None, expr, &mut var_references, &mut proc_references)?;
                    expect_n(expr.start(), 1, ret_types.len())?;
                    Some(ret_types[0].clone())
                }
                None => None,
            }
        };

        // Finally we can store the type information in the var
        let mut type_info = var.type_info.borrow_mut();
        type_info.global_var_references = Vec::from_iter(var_references);
        type_info.global_proc_references = Vec::from_iter(proc_references);
        match ret_typ {
            Some(ret_typ) => {
                if let Some(ast_typ) = &var.ast_var.typ {
                    // if explicitly specified then it must match.
                    if ast_typ.text != ret_typ {
                        return Err(Error::Mismatch {
                            loc: ast_typ.start.clone(),
                            left: ast_typ.text.clone(),
                            right: ret_typ,
                        });
                    }
                }
                type_info.typ = ret_typ;
            }
            None => match &var.ast_var.typ {
                Some(typ) => {
                    type_info.typ = typ.text.clone();
                }
                None => {
                    return Err(Error::UntypedVariable(
                        var.ast_var.name.start.clone(),
                        var.ast_var.name.text.clone(),
                    ));
                }
            },
        }
        type_info.state = CheckState::Checked;

        Ok(())
    }

    fn check_type(&self, word: &ast::Word) -> Result<(), Error> {
        // TODO: User defined types.
        if Builtin::from_str(&word.text).is_none() {
            Err(Error::UnknownType(word.start.clone(), word.text.clone()))
        } else {
            Ok(())
        }
    }

    fn check_expr(
        &self,
        proc: Option<&Proc>,
        expr: &ast::Expr,
        var_references: &mut HashSet<String>,
        proc_references: &mut HashSet<String>,
    ) -> Result<Vec<String>, Error> {
        match expr {
            ast::Expr::IntLiteral(_, _) => Ok(vec![Builtin::U64.to_str().to_string()]),
            ast::Expr::IntrinsicCall(word, _,  args) => {
                // All the intrinsics have the same signature for now.
                expect_n(&word.start, 1, args.len())?;
                self.check_expr(proc, &args[0], var_references, proc_references)?;
                Ok(vec![])
            }
            ast::Expr::Word(word) => {
                let decl = self.globals.get(&word.text);
                match decl {
                    Some(decl) => match decl {
                        Decl::Var(var) => {
                            self.check_var(var)?;
                            var_references.insert(var.ast_var.name.text.clone());
                            Ok(vec![var.type_info.borrow().typ.clone()])
                        }
                        Decl::Proc(proc) => {
                            self.check_proc(proc)?;
                            proc_references.insert(proc.ast_proc.name.text.clone());
                            Ok(proc
                                .ast_proc
                                .ret_types
                                .iter()
                                .map(|word| word.text.clone())
                                .collect::<Vec<String>>())
                        }
                    },
                    None => Err(Error::UnresolvedReference(
                        word.start.clone(),
                        word.text.clone(),
                    )),
                }
            }
            ast::Expr::Op { left, right, op } => {
                let left_types = self.check_expr(proc, left, var_references, proc_references)?;
                let right_types = self.check_expr(proc, right, var_references, proc_references)?;
                expect_n(left.start(), 1, left_types.len())?;
                expect_n(right.start(), 1, right_types.len())?;
                expect_equal(left.start(), &left_types[0], &right_types[0])?;
                if let Some(bi_typ) = Builtin::from_str(&left_types[0]) {
                    if bi_typ.supports_op(op) {
                        let typ = match op {
                            ast::Op::Sub | ast::Op::Add | ast::Op::Mul | ast::Op::Div => {
                                left_types[0].clone()
                            }
                            ast::Op::Gt | ast::Op::Lt => Builtin::Bool.to_str().to_string(),
                        };
                        return Ok(vec![typ]);
                    }
                }
                // TODO: Should this be the location of the operand?
                Err(Error::UnsupportedOp {
                    loc: left.as_ref().start().clone(),
                    typ: left_types[0].clone(),
                    op: op.clone(),
                })
            }
            ast::Expr::BracketExpr(_, expr) => {
                self.check_expr(proc, expr, var_references, proc_references)
            }
            ast::Expr::If {
                start,
                cond,
                then,
                else_,
            } => {
                self.check_cond(proc, cond, var_references, proc_references)?;
                // FIXME: Set the parent scope.
                let mut scope = Scope::new(then, None);
                self.check_block(proc, &mut scope, then, proc_references, var_references)?;
                // The kinds of expression that are allowed here are limited in the ast parsing to
                // only be if and block
                if let Some(else_) = else_ {
                    let ret_types =
                        self.check_expr(proc, else_, proc_references, var_references)?;
                    if scope.ret_types != ret_types {
                        return Err(Error::BranchMismatch {
                            loc: start.clone(),
                            first: scope.ret_types,
                            second: ret_types,
                        });
                    }
                }
                Ok(scope.ret_types)
            }
            ast::Expr::Block(block) => {
                // FIXME: Set the parent scope.
                let mut scope = Scope::new(block, None);
                self.check_block(proc, &mut scope, block, proc_references, var_references)?;
                Ok(scope.ret_types)
            }
        }
    }

    fn check_proc(&self, proc: &Proc<'a>) -> Result<(), Error> {
        {
            let type_info = proc.type_info.borrow();
            // Unlike variables, it is not an error if the type checking of this function is in
            // progress. It just means that it is recursive.
            if type_info.state != CheckState::Unchecked {
                return Ok(());
            }
        }
        {
            proc.type_info.borrow_mut().state = CheckState::InProgress;
        }

        for typ in &proc.ast_proc.ret_types {
            self.check_type(typ)?;
        }

        let mut var_references = HashSet::new();
        let mut proc_references = HashSet::new();

        let mut scope = Scope::new(&proc.ast_proc.body, None);
        self.check_block(
            Some(proc),
            &mut scope,
            &proc.ast_proc.body,
            &mut proc_references,
            &mut var_references,
        )?;

        let proc_ret_types = proc
            .ast_proc
            .ret_types
            .iter()
            .map(|word| word.text.clone())
            .collect::<Vec<String>>();
        if scope.ret_types != proc_ret_types {
            return Err(Error::InvalidReturn {
                loc: proc.ast_proc.start.clone(),
                want: proc_ret_types,
                got: scope.ret_types,
            });
        }

        {
            let mut type_info = proc.type_info.borrow_mut();
            type_info.global_var_references = Vec::from_iter(var_references);
            type_info.global_proc_references = Vec::from_iter(proc_references);
            type_info.state = CheckState::Checked;
            type_info.scope = Some(scope);
        }

        Ok(())
    }

    fn check_block(
        &self,
        proc: Option<&Proc>,
        scope: &mut Scope,
        block: &ast::Block,
        proc_references: &mut HashSet<String>,
        var_references: &mut HashSet<String>,
    ) -> Result<(), Error> {
        for stmt in &block.body {
            self.check_stmt(proc, scope, stmt, proc_references, var_references)?;
        }
        if let Some(ret_expr) = &block.ret_expr {
            scope.ret_types = self.check_expr(proc, ret_expr, var_references, proc_references)?;
        }
        Ok(())
    }

    fn check_cond(
        &self,
        proc: Option<&Proc>,
        cond: &ast::Expr,
        var_references: &mut HashSet<String>,
        proc_references: &mut HashSet<String>,
    ) -> Result<(), Error> {
        let ret_types = self.check_expr(proc, cond, var_references, proc_references)?;
        expect_n(cond.start(), 1, ret_types.len())?;
        expect_equal(
            cond.start(),
            &ret_types[0],
            &Builtin::Bool.to_str().to_string(),
        )?;
        Ok(())
    }

    fn check_stmt(
        &self,
        proc: Option<&Proc>,
        scope: &mut Scope,
        stmt: &ast::Stmt,
        proc_references: &mut HashSet<String>,
        var_references: &mut HashSet<String>,
    ) -> Result<(), Error> {
        match stmt {
            ast::Stmt::Expr(expr) => {
                let ret_types = self.check_expr(proc, expr, var_references, proc_references)?;
                expect_n(expr.start(), 0, ret_types.len())
            }
            ast::Stmt::While {
                start: _,
                cond,
                body,
            } => {
                self.check_cond(proc, cond, var_references, proc_references)?;
                self.check_block(proc, scope, body, proc_references, var_references)?;
                Ok(())
            }
            ast::Stmt::Assign(name, expr) => {
                let decl = self.globals.get(&name.text).ok_or_else(|| {
                    Error::UnresolvedReference(name.start.clone(), name.text.clone())
                })?;

                let Decl::Var(var) = decl else {
                    return Err(Error::InvalidAssign { loc: name.start.clone(), name: name.text.clone() })
                };

                self.check_var(var)?;
                var_references.insert(var.ast_var.name.text.clone());

                let ret_types = self.check_expr(proc, expr, var_references, proc_references)?;
                expect_n(expr.start(), 1, ret_types.len())?;
                expect_equal(&name.start, &var.type_info.borrow().typ, &ret_types[0])?;

                Ok(())
            }
        }
    }

    fn get_var(&self, name: &String) -> &Var {
        let decl = self.globals.get(name).expect("variable should exist");
        if let Decl::Var(var) = decl {
            var
        } else {
            panic!("Should not have got a decl that was not a var");
        }
    }

    fn get_proc(&self, name: &String) -> &Proc<'a> {
        let decl = self.globals.get(name).expect("procedure should exist");
        if let Decl::Proc(proc) = decl {
            proc
        } else {
            panic!("Should not have got a decl that was not a proc");
        }
    }
}

fn expect_equal(loc: &lexer::Loc, left: &String, right: &String) -> Result<(), Error> {
    if left == right {
        Ok(())
    } else {
        Err(Error::Mismatch {
            loc: loc.clone(),
            left: left.clone(),
            right: right.clone(),
        })
    }
}

fn expect_n(loc: &lexer::Loc, want: usize, have: usize) -> Result<(), Error> {
    if want == have {
        Ok(())
    } else {
        Err(Error::UnexpectedNumberOfTypes {
            loc: loc.clone(),
            want,
            got: have,
        })
    }
}
