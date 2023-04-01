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
        self.write_comment(file, "; -------------------- begin print intrinsic ---------")?;
        writeln!(file,
"
; inputs
; ------
; rax: uint64 to format
; rcx: base
print_uint64:
    mov rsi, print_buf         ; Set si to the start of the buffer
    call format_uint64
    mov rsi, print_buf         ; format_uint64 can change rsi

    mov     rax, 1 ; write syscall
    mov     rdi, 1 ; stdout
    mov     rsi, print_buf
    syscall

    ; Now add a new line.
    ; TODO: only make one call to write
    mov rsi, print_buf         ; format_uint64 can change rsi
    mov byte [rsi], 10
    mov     rax, 1 ; write syscall
    mov     rdi, 1 ; stdout
    mov     rdx, 1 ; len of buf
    mov     rsi, print_buf
    syscall
    ret

; inputs
; ------
; rax: uint64 to format
; rcx: base
; rsi: buffer pointer - must be big enough. Not bounds checked.
;      Might be changed through execution.
;
; returns
; ------
; rdx: The size of the buffer - no newline.
format_uint64:
    xor r8, r8 ; The number of chars in the buffer
    mov rbx, rax               ; store a copy in bx

; Increment until we have taken enough space to store the entire integer.
; Do this by dividing by 10 until the result is available;
increment_message_pointer:
    mov rdx, 0  ; We do not need the extra precision of rdx.
    div rcx

    inc rsi
    inc r8
    ; We are done if the num is now 0
    cmp rax, 0
    jne increment_message_pointer

fill_buffer_uint64:
    dec rsi

    ; We need the number again
    mov rax, rbx

add_one_char_uint64:
    mov rdx, 0  ; We do not need the extra precision of rdx.
    div rcx ; rcx has the base

    cmp rcx, 16
    je base_selection_hex

base_selection_decimal:
    call format_char_decimal
    jmp base_selection_end

base_selection_hex:
    call format_char_hex
    jmp base_selection_end

base_selection_end:

    dec rsi

    ; We are done if the result is 0
    cmp rax, 0
    jne add_one_char_uint64

; We are done so return the result
    mov     rdx, r8
    ret

; Takes value in rdx and inserts decimal char into buffer at rsi
; Works for all bases < 10
format_char_decimal:
    ; rdx stores the remainder of the devision.
    add rdx, 30h         ; 0x30 ('0')
    ; dl 8 bit portion of rdx
    mov byte [rsi], dl
    ret

; Takes value in rdx and inserts hex char into buffer at rsi
format_char_hex:
    cmp rdx, 9
    jg format_char_hex_greater_than_9
    call format_char_decimal
    jmp format_char_hex_end

format_char_hex_greater_than_9:
    ; num + 'a' - 10
    add rdx, 61h         ; or 0x61 ('a')
    sub rdx, 10
    mov byte [rsi], dl

format_char_hex_end:
    ret
"
            )?;
        self.write_comment(file, "; -------------------- end print intrinsic -----------")?;
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
        writeln!(file, "        print_buf: resb 65; 64 for binary plus newline")?;
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
                writeln!(file, "        mov rax, {}", num)?;
                Ok(())
            }
            ast::Expr::IntrinsicCall(name, exprs) => {
                self.compile_interinsic_call(name, exprs, file)
            }
            ast::Expr::Word(_) => todo!(),
            ast::Expr::Op { left, right, op } => {
                self.write_comment(
                    file,
                    "rhs evaluated into rax, moving into rbx for next operand",
                )?;
                self.compile_expr(right, file)?;
                self.write_comment(file, "lhs evaluated into rax")?;
                writeln!(file, "        mov rbx, rax")?;
                self.compile_expr(left, file)?;
                match op {
                    ast::Op::Sub => {
                        self.write_comment(file, "subtracting rhs rbx from lhs rax into rax")?;
                        writeln!(file, "        sub rax, rbx")?;
                    }
                    ast::Op::Add => {
                        self.write_comment(file, "adding lhs rax for rhs rbx into rax")?;
                        writeln!(file, "        add rax, rbx")?;
                    }
                    ast::Op::Mul => {
                        self.write_comment(file,"mul mulitplies rax by the arg and then stores the result in rax and rdx")?;
                        self.write_comment(file, "to account for overflows. We ignore overflows.")?;
                        self.write_comment(file, "lhs is already in rax.")?;
                        writeln!(file, "        mul rbx")?;
                    }
                    ast::Op::Div => {
                        self.write_comment(
                            file,
                            "div divides the dividend rdx:rax by the argument and stores the",
                        )?;
                        self.write_comment(file, "resulting quotient in rax and remainder in rdx")?;
                        self.write_comment(file,"We already have the lhs in rax, so we set rdx to zero as we do not need it.")?;
                        writeln!(file, "        mov rdx, 0")?;
                        writeln!(file, "        div rbx")?;
                    }
                    ast::Op::Gt => todo!(),
                    ast::Op::Lt => todo!(),
                }
                Ok(())
            }
            ast::Expr::BracketExpr(expr) => self.compile_expr(expr, file),
            ast::Expr::If { cond, then, else_ } => todo!(),
            ast::Expr::Block(block) => {
                self.write_comment(file, "Beginning block")?;
                for stmt in &block.body {
                    self.compile_stmt(stmt, file)?;
                }
                if let Some(ret_expr) = &block.ret_expr {
                    self.write_comment(
                        file,
                        "Block return expression.",
                    )?;
                    self.compile_expr(ret_expr, file)?;
                }
                Ok(())
            }
        }
    }

    fn write_comment(&self, file: &mut File, msg: &str) -> Result<(), CompileError> {
        // TODO: Line numbers. Pass trait into write comment what has file name and number.
        write!(file, "; ")?;
        writeln!(file, "{}", msg)?;
        Ok(())
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
                writeln!(file, "        mov rcx, 10")?; // The base
                writeln!(file, "        call print_uint64")?;
            }
            "print_hex" => {
                assert!(exprs.len() == 1, "unexpected number of arguments");
                self.compile_expr(&exprs[0], file)?;
                writeln!(file, "        mov rcx, 16")?; // The base
                writeln!(file, "        call print_uint64")?;
            }
            "print_binary" => {
                assert!(exprs.len() == 1, "unexpected number of arguments");
                self.compile_expr(&exprs[0], file)?;
                writeln!(file, "        mov rcx, 2")?; // The base
                writeln!(file, "        call print_uint64")?;
            }
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
        }
        .into())
    } else {
        Ok(())
    }
}
