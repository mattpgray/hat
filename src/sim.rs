use super::ast::*;
use std::{collections::HashMap, fmt};

pub enum ExecutionError {
    NoMainProc,
    InvalidExpressionResult {
        want: usize,
        got: usize,
    },
    InvalidArgs {
        name: String,
        want: usize,
        got: usize,
    },
}

impl fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExecutionError::NoMainProc => write!(f, "no main procedure"),
            ExecutionError::InvalidArgs { name, want, got } => {
                write!(
                    f,
                    "invalid number of arguments for {}: want {}, got {}",
                    name, want, got
                )
            }
            ExecutionError::InvalidExpressionResult { want, got } => {
                write!(f, "invalid expression result: want {}, got {}", want, got)
            }
        }
    }
}

struct ExprResult {
    results: Vec<u64>,
}

fn expect_expr_result(result: &ExprResult, want: usize) -> Result<(), ExecutionError> {
    if result.results.len() != want {
        return Err(ExecutionError::InvalidExpressionResult {
            want,
            got: result.results.len(),
        });
    }
    Ok(())
}

#[derive(Default)]
pub struct Context {
    global_vars: HashMap<String, u64>,
}

impl Context {
    pub fn run(&mut self, ast: Ast) -> Result<(), ExecutionError> {
        for (_, val) in ast.global_vars.iter() {
            self.global_vars
                .insert(val.name.clone(), val.value.unwrap_or(0));
        }
        let main = match ast.procs.get("main") {
            Some(proc) => proc,
            None => return Err(ExecutionError::NoMainProc),
        };
        self.run_proc(main)
    }

    fn run_proc(&mut self, proc: &Proc) -> Result<(), ExecutionError> {
        self.run_stmts(&proc.body)
    }

    fn run_stmts(&mut self, stmts: &Vec<Stmt>) -> Result<(), ExecutionError> {
        for stmt in stmts {
            self.run_stmt(stmt)?;
        }
        Ok(())
    }

    fn run_stmt(&mut self, stmt: &Stmt) -> Result<(), ExecutionError> {
        match stmt {
            Stmt::Expr(expr) => {
                self.run_expr(expr)?;
                Ok(())
            }
            Stmt::Block(block) => self.run_stmts(block),
            Stmt::If { cond, then, else_ } => self.run_if(cond, then, else_),
        }
    }

    fn run_if(
        &mut self,
        cond: &Expr,
        then: &Vec<Stmt>,
        else_: &Option<Box<Stmt>>,
    ) -> Result<(), ExecutionError> {
        let cond_result = self.run_expr(cond)?;
        expect_expr_result(&cond_result, 1)?;
        if cond_result.results[0] != 0 {
            self.run_stmts(then)
        } else if let Some(else_) = else_ {
            self.run_stmt(else_)
        } else {
            Ok(())
        }
    }

    fn run_expr(&mut self, expr: &Expr) -> Result<ExprResult, ExecutionError> {
        match expr {
            Expr::IntLiteral(u) => Ok(ExprResult { results: vec![*u] }),
            Expr::IntrinsicCall(name, args) => self.run_intrinsic(name, args),
        }
    }

    fn run_intrinsic(
        &mut self,
        name: &str,
        args: &Vec<Expr>,
    ) -> Result<ExprResult, ExecutionError> {
        match name {
            "print" => {
                if args.len() != 1 {
                    return Err(ExecutionError::InvalidArgs {
                        name: format!("#{}", name),
                        want: 1,
                        got: args.len(),
                    });
                }
                let evaluated_args = self.run_args(args)?;
                assert_eq!(evaluated_args.len(), 1);

                println!("{}", evaluated_args[0]);
                Ok(ExprResult {
                    results: Vec::new(),
                })
            }
            _ => Err(ExecutionError::NoMainProc),
        }
    }

    fn run_args(&mut self, args: &Vec<Expr>) -> Result<Vec<u64>, ExecutionError> {
        let mut results = Vec::new();
        for arg in args {
            let arg_result = self.run_expr(arg)?;
            expect_expr_result(&arg_result, 1)?;
            results.extend(arg_result.results);
        }
        Ok(results)
    }
}
