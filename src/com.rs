use super::ast;
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
        cmd("ld", &["out.o", "-o", "out"])?;
        Ok(())
    }

    fn compile_print(&self, file: &mut File) -> Result<(), CompileError> {
        writeln!(file, "print_hex_uint:")?;
        writeln!(
            file,
            "    xor rdx, rdx               ; The number of characters in the buffer"
        )?;
        writeln!(
            file,
            "    mov rsi, print_buf         ; si points to the target buffer"
        )?;
        writeln!(file, "    mov rbx, rax               ; store a copy in bx")?;
        writeln!(file, "")?;
        writeln!(file, "    cmp rax, 0")?;
        writeln!(file, "    jne  start_strip_leading_zeros")?;
        writeln!(file, "    ; Special case for 0")?;
        writeln!(file, "    mov byte [rsi], 030h")?;
        writeln!(file, "    inc rsi")?;
        writeln!(file, "    inc rdx")?;
        writeln!(file, "    jmp done_conversion")?;
        writeln!(file, "")?;
        writeln!(file, "strip_leading_zeros:")?;
        writeln!(
            file,
            "    shl rbx, 4                      ; get the next part"
        )?;
        writeln!(file, "")?;
        writeln!(file, "start_strip_leading_zeros:")?;
        writeln!(file, "    mov rax, rbx          ; load the number into ax")?;
        writeln!(file, "    shr rax, 60           ; grap the last byte")?;
        writeln!(file, "    jz strip_leading_zeros ; if this is zero then keep stripping. Otherwise continue to printing below")?;
        writeln!(file, "")?;
        writeln!(file, "convert_loop:")?;
        writeln!(file, "    mov rax, rbx          ; load the number into ax")?;
        writeln!(file, "    shr rax, 60           ; grab the last byte")?;
        writeln!(file, "")?;
        writeln!(file, "    cmp rax, 9h          ; check what we should add")?;
        writeln!(file, "    jg  greater_than_9")?;
        writeln!(file, "    add rax, 30h         ; 0x30 ('0')")?;
        writeln!(file, "    jmp converted")?;
        writeln!(file, "")?;
        writeln!(file, "greater_than_9:")?;
        writeln!(file, "    ; num + 'a' - 10")?;
        writeln!(file, "    add rax, 61h         ; or 0x61 ('a')")?;
        writeln!(file, "    sub rax, 10")?;
        writeln!(file, "")?;
        writeln!(file, "converted:")?;
        writeln!(file, "    mov [rsi], rax")?;
        writeln!(file, "    inc rsi")?;
        writeln!(file, "    inc rdx")?;
        writeln!(file, "    shl rbx, 4           ; get the next part")?;
        writeln!(file, "    jnz convert_loop")?;
        writeln!(file, "")?;
        writeln!(file, "done_conversion:")?;
        writeln!(file, "    mov byte [rsi], 10 ; new line")?;
        writeln!(file, "    inc rdx")?;
        writeln!(file, "")?;
        writeln!(file, "    mov     rax, 1")?;
        writeln!(file, "    mov     rdi, 1")?;
        writeln!(file, "    mov     rsi, print_buf")?;
        writeln!(file, "    syscall")?;
        writeln!(file, "    ret")?;
        Ok(())
    }

    fn compile_ast(&self, ast: &ast::Ast, file: &mut File) -> Result<(), CompileError> {
        writeln!(file, "section .text")?;
        writeln!(file, "global _start")?;
        // Hard coded print intrinsic - also uses the print buf below
        self.compile_print(file)?;

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
        writeln!(file, "section .bss")?;
        writeln!(file, "        print_buf: resb 17")?;
        Ok(())
    }

    fn compile_global_variable(&self, var: &ast::Var, file: &mut File) -> Result<(), CompileError> {
        todo!("compile_global_variable is not implemented yet");
    }

    fn compile_proc(&self, proc: &ast::Proc, file: &mut File) -> Result<(), CompileError> {
        writeln!(file, "{}:", proc.name)?;
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
            ast::Expr::IntLiteral(num) => {
                let num = hex_num(num.to_owned());
                writeln!(file, "        mov rax, {}", num)?;
                Ok(())
            }
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
            ast::Stmt::Expr(expr) => self.compile_expr(expr, file),
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
                assert!(exprs.len() == 1, "unexpected number of arguments");
                self.compile_expr(&exprs[0], file)?;
                writeln!(file, "        call print_hex_uint")?;
            }
            // This willbe caught during type checking eventually. No need for a good error now.
            _ => panic!("unknown intrinsic: {}", name),
        }
        Ok(())
    }
}

fn hex_num(num: u64) -> String {
    let mut sb = String::new();
    let mut num = num;
    loop {
        let remainder: u8 = u8::try_from(num % 16).expect("Should fit into a u8");
        num = num / 16;
        let char = if remainder < 10 {
            '0' as u8 + remainder
        } else {
            'a' as u8 + remainder - 10
        };
        sb.insert(0, char as char);
        if num == 0 {
            break;
        }
    }
    sb
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
        }
        .into())
    } else {
        Ok(())
    }
}
