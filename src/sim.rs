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
    UknownValue(String),
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
            ExecutionError::UknownValue(word) => write!(f, "unknown value: '{}'", word),
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
    global_procs: HashMap<String, Proc>,
}

impl Context {
    pub fn run(&mut self, ast: Ast) -> Result<(), ExecutionError> {
        for decl in ast.decls.iter() {
            match decl {
                Decl::Proc(proc) => {
                    self.global_procs.insert(proc.name.clone(), proc.clone());
                }
                Decl::Var(var) => {
                    let res = match var.value.as_ref() {
                        Some(val) => {
                            let expr_res = self.run_expr(val)?;
                            expect_expr_result(&expr_res, 1)?;
                            expr_res.results[0]
                        }
                        None => 0,
                    };
                    self.global_vars.insert(var.name.clone(), res);
                }
            }
        }
        // Get the main proc and then run the program from the main proc
        let main_proc = self
            .global_procs
            .get("main")
            .ok_or(ExecutionError::NoMainProc)?
            .clone();
        self.run_proc(&main_proc)
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
            Stmt::Assign(name, expr) => self.run_assign(name, expr),
            Stmt::While { cond, body } => self.run_while(cond, body),
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

    fn run_while(&mut self, cond: &Expr, body: &Vec<Stmt>) -> Result<(), ExecutionError> {
        let mut cond_result = self.run_expr(cond)?;
        expect_expr_result(&cond_result, 1)?;
        while cond_result.results[0] != 0 {
            self.run_stmts(body)?;
            cond_result = self.run_expr(cond)?;
            expect_expr_result(&cond_result, 1)?;
        }
        Ok(())
    }

    fn run_assign(&mut self, name: &str, expr: &Expr) -> Result<(), ExecutionError> {
        let result = self.run_expr(expr)?;
        expect_expr_result(&result, 1)?;
        self.global_vars.insert(name.to_owned(), result.results[0]);
        Ok(())
    }

    fn run_expr(&mut self, expr: &Expr) -> Result<ExprResult, ExecutionError> {
        match expr {
            Expr::IntLiteral(u) => Ok(ExprResult { results: vec![*u] }),
            Expr::IntrinsicCall(name, args) => self.run_intrinsic(name, args),
            Expr::Word(word) => {
                let v = self
                    .global_vars
                    .get(word)
                    .ok_or_else(|| ExecutionError::UknownValue(word.clone()))?;
                Ok(ExprResult { results: vec![*v] })
            }
            Expr::Op { left, right, op } => self.run_op(left, right, op),
            Expr::BracketExpr(expr) => self.run_expr(expr),
        }
    }

    fn run_op(&mut self, left: &Expr, right: &Expr, op: &Op) -> Result<ExprResult, ExecutionError> {
        let left_result = self.run_expr(left)?;
        let right_result = self.run_expr(right)?;
        expect_expr_result(&left_result, 1)?;
        expect_expr_result(&right_result, 1)?;
        let mut results = vec![];
        match op {
            Op::Sub => results.push(left_result.results[0] - right_result.results[0]),
            Op::Add => results.push(left_result.results[0] + right_result.results[0]),
            Op::Mul => results.push(left_result.results[0] * right_result.results[0]),
            Op::Div => results.push(left_result.results[0] / right_result.results[0]),
            Op::Gt => results.push(if left_result.results[0] > right_result.results[0] {
                1
            } else {
                0
            }),
            Op::Lt => results.push(if left_result.results[0] < right_result.results[0] {
                1
            } else {
                0
            }),
        }
        Ok(ExprResult { results })
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
