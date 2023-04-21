use std::collections::{HashMap, HashSet};

use crate::{
    ast::{self, IntrinsicName, Node},
    lexer,
};

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
    NoMainFunction,
}

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

pub struct Var<'a> {
    pub ast_var: &'a ast::Var,
    pub state: CheckState,
    pub typ: String,
    pub global_proc_references: Vec<String>,
    pub global_var_references: Vec<String>,
}

impl Var<'_> {
    pub fn underlying_typ(&self) -> Builtin {
        Builtin::from_str(&self.typ).expect("Parsed var should have valid type")
    }
}

pub struct Proc<'a> {
    ast_proc: &'a ast::Proc,
}

#[derive(PartialEq, Eq)]
pub enum CheckState {
    Checked,
    InProgress,
    Unchecked,
}

pub struct Context<'a> {
    pub globals: HashMap<String, Decl<'a>>,
    pub ast: &'a ast::Ast,
}

impl Context<'_> {
    // If from does not return an error, the modes that use the type context can rely on the at
    // being valid.
    pub fn from<'a>(ast: &'a ast::Ast) -> Result<Context<'a>, Error> {
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
                        state: CheckState::Unchecked,
                        typ: "".to_string(),
                        global_proc_references: vec![],
                        global_var_references: vec![],
                    }),
                ),
                ast::Decl::Proc(proc) => (&proc.name, Decl::Proc(Proc { ast_proc: proc })),
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
                context.check_var(&var.name.text)?;
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
        let references = {
            let decl = self.globals.get(key).expect("key exists");
            match decl {
                Decl::Var(var) => &var.global_var_references,
                _ => return Ok(()),
            }
        };

        for reference in references {
            self.walk_var_declarations_impl(reference, visited, f)?;
        }

        let var = self.get_var(key);
        f(var)?;
        visited.insert(key.to_owned());

        Ok(())
    }

    fn check_var(&mut self, name: &String) -> Result<(), Error> {
        // First check for cycles and set the var as in progres. Then get the references to other
        // global declarations that are in the variable definition.
        let references = {
            self.mut_var(name, |var| {
                if var.state == CheckState::InProgress {
                    return Err(Error::InitializationCycle(
                        var.ast_var.name.start.clone(),
                        name.clone(),
                    ));
                } else if var.state == CheckState::Unchecked {
                    var.state = CheckState::InProgress;
                }
                Ok(())
            })?;
            let var = self.get_var(name);
            if var.state == CheckState::Checked {
                return Ok(());
            }
            self.var_references(var)?
        };

        // Not add those references to the var
        let mut proc_references = Vec::new();
        let mut var_references = Vec::new();
        for reff in references {
            let decl = self
                .globals
                .get(&reff)
                .expect("reference should be in globals");
            match decl {
                Decl::Var(_) => var_references.push(reff),
                Decl::Proc(_) => proc_references.push(reff),
            }
        }
        self.mut_var(name, |var| {
            var.global_var_references = var_references.clone();
            var.global_proc_references = proc_references.clone();
            Ok(())
        })?;

        // Now we type check the other variables and procs recursively.
        for name in &var_references {
            self.check_var(name)?;
        }
        for name in &proc_references {
            // We do not type check the proc here. We just check for variable cycles within it.
            self.check_vars_in_proc(name)?;
        }

        // And finally we can type check the variable we have. All other variables that this
        // depends on should already by type checked.
        let ret_typ = {
            let var = self.get_var(name);
            // If the type is declared then it must be valid.
            if let Some(typ) = &var.ast_var.typ {
                self.check_type(typ)?;
            }
            match &var.ast_var.value {
                Some(expr) => {
                    let ret_types = self.check_expr(expr)?;
                    expect_n(expr.start(), 1, ret_types.len())?;
                    Some(ret_types[0].clone())
                }
                None => None,
            }
        };

        self.mut_var(name, |var| {
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
                    var.typ = ret_typ;
                }
                None => match &var.ast_var.typ {
                    Some(typ) => {
                        var.typ = typ.text.clone();
                    }
                    None => {
                        return Err(Error::UntypedVariable(
                            var.ast_var.name.start.clone(),
                            name.clone(),
                        ));
                    }
                },
            }
            var.state = CheckState::Checked;
            Ok(())
        })?;

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

    fn check_expr(&self, expr: &ast::Expr) -> Result<Vec<String>, Error> {
        match expr {
            ast::Expr::IntLiteral(_, _) => Ok(vec![Builtin::U64.to_str().to_string()]),
            ast::Expr::IntrinsicCall(IntrinsicName { hash_loc, .. }, _) => {
                todo!("{hash_loc}: Type checking of intrinsic calls in expressions is not implemented yet")
            }
            ast::Expr::Word(word) => {
                let decl = self.globals.get(&word.text);
                match decl {
                    Some(decl) => match decl {
                        Decl::Var(var) => {
                            assert!(var.state == CheckState::Checked);
                            Ok(vec![var.typ.clone()])
                        }
                        Decl::Proc(proc) => Ok(proc.ast_proc.ret_types.clone()),
                    },
                    None => Err(Error::UnresolvedReference(
                        word.start.clone(),
                        word.text.clone(),
                    )),
                }
            }
            ast::Expr::Op { left, right, op } => {
                let left_types = self.check_expr(left)?;
                let right_types = self.check_expr(right)?;
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
            ast::Expr::BracketExpr(_, expr) => self.check_expr(expr),
            ast::Expr::If {
                start: _,
                cond: _,
                then: _,
                else_: _,
            } => todo!("type checking if is not implemented yet"),
            ast::Expr::Block(_) => todo!("Type checking block is not implemented yet"),
        }
    }

    fn check_vars_in_proc(&mut self, name: &String) -> Result<(), Error> {
        todo!("Checking for inintialization cyclces within function bodies is not implemented");
    }

    fn var_references(&self, var: &Var) -> Result<HashSet<String>, Error> {
        let mut references = HashSet::new();
        if let Some(expr) = &var.ast_var.value {
            self.append_references(expr, &mut references)?;
        }
        Ok(references)
    }

    fn append_references(
        &self,
        expr: &ast::Expr,
        references: &mut HashSet<String>,
    ) -> Result<(), Error> {
        match expr {
            ast::Expr::IntLiteral(_, _) | ast::Expr::IntrinsicCall(_, _) => Ok(()),
            ast::Expr::Word(word) => {
                if !self.globals.contains_key(&word.text) {
                    Err(Error::UnresolvedReference(
                        word.start.clone(),
                        word.text.clone(),
                    ))
                } else {
                    references.insert(word.text.clone());
                    Ok(())
                }
            }
            ast::Expr::Op { left, right, op: _ } => {
                self.append_references(left, references)?;
                self.append_references(right, references)?;
                Ok(())
            }
            ast::Expr::BracketExpr(_, expr) => self.append_references(expr, references),
            ast::Expr::If {
                start: _,
                cond: _,
                then: _,
                else_: _,
            } => {
                todo!("Getting references from if is not implemented")
            }
            ast::Expr::Block(_) => todo!("Getting references from blocks is not implemented"),
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

    // mut_var looks weird and that is because of rust being a pain in the ass. We cannot get
    // another reference to any member of the hashset if we have a mutable reference to one of
    // them. This function allows for the most common use case, mutating the hashset when we know
    // that the key contains a variable declaration.
    fn mut_var<F>(&mut self, name: &String, f: F) -> Result<(), Error>
    where
        F: FnOnce(&mut Var) -> Result<(), Error>,
    {
        let decl = self.globals.get_mut(name).expect("variable should exist");
        if let Decl::Var(var) = decl {
            f(var)
        } else {
            panic!("Should not have got a decl that was not a var");
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
