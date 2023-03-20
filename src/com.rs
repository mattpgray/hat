use super::ast;
use super::lexer;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

#[derive(Default)]
pub struct Compiler {}

impl Compiler {
    pub fn compile(&self, out_file: &str, ast: &ast::Ast) -> Result<(), CompileError> {
        {
            let mut out = File::create("out.asm")?;
            self.compile_ast(&ast, &mut out)?;
        }
        cmd("nasm", &["-felf64", "out.asm"])?;
        cmd("ld", &["out.o", "-o", out_file])?;
        Ok(())
    }

    fn compile_ast(&self, ast: &ast::Ast, file: &mut File) -> Result<(), CompileError> {
        writeln!(file, "        global _start")?;
        writeln!(file, "")?;
        writeln!(file, "        section .text")?;

        // Hard coded print intrinsic
        writeln!(file, "print:  mov     rax, 1")?;
        writeln!(file, "        mov     rdi, 1")?;
        writeln!(file, "        mov     rsi, message")?;
        writeln!(file, "        mov     rdx, messagelen")?;
        writeln!(file, "        syscall")?;
        writeln!(file, "        ret")?;
        writeln!(file, "      ")?;

        for decl in &ast.decls {
            match decl {
                ast::Decl::Var(var) => self.compile_global_variable(var, file)?,
                ast::Decl::Proc(proc) => self.compile_proc(proc, file)?,
            }
        }

        writeln!(file, "_start:")?;
        writeln!(file, "        call main")?;
        writeln!(file, "        mov     rax, 60")?;
        writeln!(file, "        xor     rdi, rdi")?;
        writeln!(file, "        syscall")?;
        writeln!(file, "")?;
        writeln!(file, "        section .data")?;
        writeln!(file, "message: db     \"Hello, world\", 10")?;
        writeln!(file, "messagelen: EQU $ - message")?;
        Ok(())
    }

    fn compile_global_variable(&self, var: &ast::Var, file: &mut File) -> Result<(), CompileError> {
        todo!("compile_global_variable is not implemented yet");
    }

    fn compile_proc(&self, proc: &ast::Proc, file: &mut File) -> Result<(), CompileError> {
        writeln!(file, "{}:", proc.name);
        self.compile_block(&proc.body, file)?;
        writeln!(file, "        ret")?;
        Ok(())
    }

    fn compile_block(&self, block: &ast::Block, file: &mut File) -> Result<(), CompileError> {
        for stmt in &block.body {
            self.compile_stmt(stmt, file)?;
        }
        if let Some(_) = block.ret_expr {
            todo!("parsing of block return bodies is not implented yet");
        }
        Ok(())
    }

    fn compile_expr(&self, expr: &ast::Expr, file: &mut File) -> Result<(), CompileError> {
        match expr {
            ast::Expr::IntLiteral(_) => todo!(),
            ast::Expr::IntrinsicCall(name, exprs) => {
                self.compile_interinsic_call(name, exprs, file)
            }
            ast::Expr::Word(_) => todo!(),
            ast::Expr::Op { left, right, op } => todo!(),
            ast::Expr::BracketExpr(_) => todo!(),
            ast::Expr::If { cond, then, else_ } => todo!(),
            ast::Expr::Block(_) => todo!(),
        }
    }

    fn compile_stmt(&self, stmt: &ast::Stmt, file: &mut File) -> Result<(), CompileError> {
        match stmt {
            ast::Stmt::Expr(expr) =>self.compile_expr(expr, file),
            ast::Stmt::While { cond, body } => todo!(),
            ast::Stmt::Assign(_, _) => todo!(),
        }
    }

    fn compile_interinsic_call(
        &self,
        name: &str,
        exprs: &Vec<ast::Expr>,
        file: &mut File,
    ) -> Result<(), CompileError> {
        match name {
            "print" => {
               writeln!(file, "        call print")?;
            },
            // This willbe caught during type checking eventually. No need for a good error now.
            _ => panic!("unknown intrinsic: {}", name),
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum CompileError {
    CmdError(CmdError),
    Io(std::io::Error),
}

impl From<CmdError> for CompileError {
    fn from(err: CmdError) -> Self {
        CompileError::CmdError(err)
    }
}

impl From<std::io::Error> for CompileError {
    fn from(err: std::io::Error) -> Self {
        CompileError::Io(err)
    }
}

#[derive(Debug)]
pub struct CmdError {
    pub name: String,
    pub code: i32,
    pub message: String,
}

fn cmd(name: &str, args: &[&str]) -> Result<(), CompileError> {
    // TODO: escape
    print!("[cmd] {}", name);
    for arg in args {
        print!(" {}", arg);
    }
    println!();
    let mut cmd = Command::new(name);
    cmd.args(args);
    let output = cmd.output()?;
    if !output.status.success() {
        Err(CmdError {
            name: name.to_string(),
            code: output.status.code().unwrap_or(-1),
            message: String::from_utf8_lossy(&output.stderr).into_owned(),
        }.into())
    } else {
        Ok(())
    }
}
