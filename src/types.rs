use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

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
    // type_info contains information about the var that is discovered at type checking time. It
    // it in a ref cell so that we can mutate it more easily as we traverse the tree.
    pub type_info: RefCell<VarTypeInformation>,
}

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
                        type_info: RefCell::new(VarTypeInformation {
                            state: CheckState::Unchecked,
                            typ: "".to_string(),
                            global_proc_references: vec![],
                            global_var_references: vec![],
                        }),
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
                let var = context.get_var(&var.name.text);
                context.check_var(var)?;
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

        // Release the borrow as we check other variable when getting the references.
        let references = self.var_references(var)?;

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

        // Now we can store the references.
        {
            let mut type_info = var.type_info.borrow_mut();
            type_info.global_var_references = var_references.clone();
            type_info.global_proc_references = proc_references.clone();
        }

        // Now we type check the other variables and procs recursively.
        for name in &var_references {
            self.check_var(self.get_var(name))?;
        }
        for name in &proc_references {
            // We do not type check the proc here. We just check for variable cycles within it.
            self.check_vars_in_proc(self.get_proc(name))?;
        }

        // And finally we can type check the variable we have. All other variables that this
        // depends on should already by type checked.
        let ret_typ = {
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

        // Finally we can store the type information in the var
        let mut type_info = var.type_info.borrow_mut();
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
                            assert!(var.type_info.borrow().state == CheckState::Checked);
                            Ok(vec![var.type_info.borrow().typ.clone()])
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

    fn check_vars_in_proc(&self, proc: &Proc) -> Result<(), Error> {
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

    fn get_proc(&self, name: &String) -> &Proc {
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
