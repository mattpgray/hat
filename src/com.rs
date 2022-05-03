use super::ast;
use super::lexer;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

#[derive(Default)]
pub struct Compiler {}

impl Compiler {
    pub fn compile(&self, src_file: &str) -> Result<(), CompileError> {
        let src_data = fs::read_to_string(src_file).unwrap();
        let mut l = lexer::Lexer::new(src_data.chars(), src_file.to_string());
        let ast = ast::Ast::parse(&mut l).unwrap();
        {
            let mut out = File::create("out.asm").unwrap();
            self.compile_ast(&ast, &mut out).unwrap();
        }
        cmd("nasm", &["-felf64", "out.asm"]).unwrap();
        cmd("ld", &["out.o", "-o", "out"]).unwrap();
        Ok(())
    }

    fn compile_ast(&self, ast: &ast::Ast, file: &mut File) -> Result<(), CompileError> {
        file.write_all(b"        global _start\n")?;
        file.write_all(b"\n")?;
        file.write_all(b"        section .text\n")?;
        file.write_all(b"print: mov     rax, 1\n")?;
        file.write_all(b"        mov     rdi, 1\n")?;
        file.write_all(b"        mov     rsi, message2\n")?;
        file.write_all(b"        mov     rdx, message2len\n")?;
        file.write_all(b"        syscall\n")?;
        file.write_all(b"        ret\n")?;
        file.write_all(b"      \n")?;
        file.write_all(b"_start: mov     rax, 1\n")?;
        file.write_all(b"        mov     rdi, 1\n")?;
        file.write_all(b"        mov     rsi, message\n")?;
        file.write_all(b"        mov     rdx, messagelen\n")?;
        file.write_all(b"        syscall\n")?;
        file.write_all(b"        call print\n")?;
        file.write_all(b"        mov     rax, 60\n")?;
        file.write_all(b"        xor     rdi, rdi\n")?;
        file.write_all(b"        syscall\n")?;
        file.write_all(b"\n")?;
        file.write_all(b"        section .data\n")?;
        file.write_all(b"message: db     \"Hello, world\", 10\n")?;
        file.write_all(b"messagelen: EQU $ - message\n")?;
        file.write_all(b"        \n")?;
        file.write_all(b"message2: db     \"Hello, amigo\", 10\n")?;
        file.write_all(b"message2len: EQU $ - message\n")?;
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
    code: i32,
    message: String,
}

fn cmd(name: &str, args: &[&str]) -> Result<(), CmdError> {
    // TODO: escape
    print!("[cmd] {}", name);
    for arg in args {
        print!(" {}", arg);
    }
    println!();
    let mut cmd = Command::new(name);
    cmd.args(args);
    let output = cmd.output().map_err(|e| e.to_string()).unwrap();
    if !output.status.success() {
        Err(CmdError {
            code: output.status.code().unwrap_or(-1),
            message: String::from_utf8_lossy(&output.stderr).into_owned(),
        })
    } else {
        Ok(())
    }
}
