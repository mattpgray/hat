use std::collections::{HashMap, HashSet};

use crate::ast;

pub enum Builtin {
    U64,
}

impl Builtin {
    fn from_str(str: &str) -> Option<Builtin> {
        match str {
            "u64" => Some(Builtin::U64),
            _ => None,
        }
    }

    fn supports_op(&self, op: &ast::Op) -> bool {
        true // TODO: This will not always be true.
    }

    fn to_str(&self) -> &str {
        match self {
            Builtin::U64 => "u64",
        }
    }
}

#[derive(Debug)]
pub enum Error {
    InitializationCycle(String),
    UnresolvedReference(String),
    UnsupportedOp { typ: String, op: ast::Op },
    Mismatch { left: String, right: String },
    UntypedVariable(String),
    UnknownType(String),
    UnexpectedNumberOfTypes { want: usize, got: usize },
}

enum Decl<'a> {
    Var(Var<'a>),
    Proc(Proc<'a>),
}

struct Var<'a> {
    ast_var: &'a ast::Var,
    state: CheckState,
    typ: String,
    global_proc_references: Vec<String>,
    global_var_references: Vec<String>,
}

struct Proc<'a> {
    ast_proc: &'a ast::Proc,
}

#[derive(PartialEq, Eq)]
enum CheckState {
    Checked,
    InProgress,
    Unchecked,
}

pub struct Context<'a> {
    globals: HashMap<String, Decl<'a>>,
}

impl Context<'_> {
    pub fn from<'a>(ast: &'a ast::Ast) -> Result<Context<'a>, Error> {
        let mut context = Context {
            globals: HashMap::new(),
        };
        for decl in &ast.decls {
            match decl {
                ast::Decl::Var(var) => {
                    context.globals.insert(
                        var.name.clone(),
                        Decl::Var(Var {
                            ast_var: var,
                            state: CheckState::Unchecked,
                            typ: "".to_string(),
                            global_proc_references: vec![],
                            global_var_references: vec![],
                        }),
                    );
                }
                ast::Decl::Proc(proc) => {
                    context
                        .globals
                        .insert(proc.name.clone(), Decl::Proc(Proc { ast_proc: proc }));
                }
            }
        }
        for decl in &ast.decls {
            if let ast::Decl::Var(var) = decl {
                context.check_var(&var.name)?;
            }
        }

        Ok(context)
    }

    fn check_var(&mut self, name: &String) -> Result<(), Error> {
        // First check for cycles and set the var as in progres. Then get the references to other
        // global declarations that are in the variable definition.
        let references = {
            self.mut_var(name, |var| {
                if var.state == CheckState::InProgress {
                    return Err(Error::InitializationCycle(name.clone()));
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
                    expect_n(1, ret_types.len())?;
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
                        if ast_typ != &ret_typ {
                            return Err(Error::Mismatch {
                                left: ast_typ.clone(),
                                right: ret_typ,
                            });
                        }
                    }
                    var.typ = ret_typ;
                }
                None => match &var.ast_var.typ {
                    Some(typ) => {
                        var.typ = typ.clone();
                    }
                    None => {
                        return Err(Error::UntypedVariable(name.clone()));
                    }
                },
            }
            var.state = CheckState::Checked;
            Ok(())
        })?;

        Ok(())
    }

    fn check_type(&self, typ: &String) -> Result<(), Error> {
        // TODO: User defined types.
        if Builtin::from_str(typ).is_none() {
            Err(Error::UnknownType(typ.clone()))
        } else {
            Ok(())
        }
    }

    fn check_expr(&self, expr: &ast::Expr) -> Result<Vec<String>, Error> {
        match expr {
            ast::Expr::IntLiteral(_) => Ok(vec![Builtin::U64.to_str().to_string()]),
            ast::Expr::IntrinsicCall(_, _) => {
                todo!("Type checking of intrinsic calls in expressions is not implemented yet")
            }
            ast::Expr::Word(word) => {
                let decl = self.globals.get(word);
                match decl {
                    Some(decl) => match decl {
                        Decl::Var(var) => {
                            assert!(var.state == CheckState::Checked);
                            Ok(vec![var.typ.clone()])
                        }
                        Decl::Proc(proc) => Ok(proc.ast_proc.ret_types.clone()),
                    },
                    None => Err(Error::UnresolvedReference(word.clone())),
                }
            }
            ast::Expr::Op { left, right, op } => {
                let left = self.check_expr(left)?;
                let right = self.check_expr(right)?;
                expect_n(1, left.len())?;
                expect_n(1, right.len())?;
                expect_equal(&left[0], &right[0])?;
                if let Some(bi_typ) = Builtin::from_str(&left[0]) {
                    if bi_typ.supports_op(op) {
                        return Ok(left);
                    }
                }
                Err(Error::UnsupportedOp {
                    typ: left[0].clone(),
                    op: op.clone(),
                })
            }
            ast::Expr::BracketExpr(expr) => self.check_expr(expr),
            ast::Expr::If { cond, then, else_ } => todo!("type checking if is not implemented yet"),
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
            ast::Expr::IntLiteral(_) | ast::Expr::IntrinsicCall(_, _) => Ok(()),
            ast::Expr::Word(word) => {
                if !self.globals.contains_key(word) {
                    Err(Error::UnresolvedReference(word.clone()))
                } else {
                    references.insert(word.clone());
                    Ok(())
                }
            }
            ast::Expr::Op { left, right, op } => {
                self.append_references(left, references)?;
                self.append_references(right, references)?;
                Ok(())
            }
            ast::Expr::BracketExpr(expr) => self.append_references(expr, references),
            ast::Expr::If { cond, then, else_ } => {
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

fn expect_equal(left: &String, right: &String) -> Result<(), Error> {
    if left == right {
        Ok(())
    } else {
        Err(Error::Mismatch {
            left: left.clone(),
            right: right.clone(),
        })
    }
}

fn expect_n(want: usize, have: usize) -> Result<(), Error> {
    if want == have {
        Ok(())
    } else {
        Err(Error::UnexpectedNumberOfTypes { want, got: have })
    }
}
