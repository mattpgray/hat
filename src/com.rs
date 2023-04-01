use super::ast;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

#[derive(Default)]
pub struct Compiler {
    n_jumps: usize, // Incremented as we go
}

impl Compiler {
    pub fn compile(&mut self, out_file: String, ast: &ast::Ast) -> Result<(), CompileError> {
        {
            let mut out = File::create("out.asm")?;
            self.compile_ast(ast, &mut out)?;
        }
        cmd("nasm", &["-felf64", "out.asm"])?;
        cmd("ld", &["out.o", "-o", out_file.as_str()])?;
        Ok(())
    }

    fn compile_print(&self, file: &mut File) -> Result<(), CompileError> {
        self.write_comment(
            file,
            "; -------------------- begin print intrinsic ---------",
        )?;
        writeln!(
            file,
            "
; inputs
; ------
; rax: uint64 to format
; rcx: base
print_uint64:
    mov rsi, print_buf         ; Set si to the start of the buffer
    call format_uint64
    mov rsi, print_buf         ; format_uint64 can change rsi

    ; Add the newline at the end of the buf
    mov rbx, rdx
    add rbx, rsi
    mov byte [rbx], 10
    inc rdx
    ; Call write syscall
    mov     rax, 1 ; write syscall
    mov     rdi, 1 ; stdout
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
        self.write_comment(
            file,
            "; -------------------- end print intrinsic -----------",
        )?;
        Ok(())
    }

    fn compile_ast(&mut self, ast: &ast::Ast, file: &mut File) -> Result<(), CompileError> {
        self.compile_text(ast, file)?;
        self.compile_data(ast, file)?;
        self.compile_bss(ast, file)?;
        Ok(())
    }

    fn compile_text(&mut self, ast: &ast::Ast, file: &mut File) -> Result<(), CompileError> {
        writeln!(file, "section .text")?;
        writeln!(file, "global _start")?;
        // Hard coded print intrinsic - also uses the print buf compile_bss
        self.compile_print(file)?;

        for decl in &ast.decls {
            match decl {
                ast::Decl::Proc(proc) => self.compile_proc(proc, file)?,
                ast::Decl::Var(_) => continue,
            }
        }

        writeln!(file, "_start:")?;
        self.write_comment(
            file,
            "------- begin global variable initialization --------",
        )?;
        for decl in &ast.decls {
            match decl {
                ast::Decl::Proc(_) => continue,
                ast::Decl::Var(var) => self.compile_global_variable_initialization(var, file)?,
            }
        }
        self.write_comment(
            file,
            "------- end global variable initialization ----------",
        )?;

        writeln!(file, "        call main")?;
        writeln!(file, "        mov     rax, 60")?;
        writeln!(file, "        xor     rdi, rdi")?;
        writeln!(file, "        syscall")?;
        writeln!(file)?;
        Ok(())
    }

    fn compile_data(&mut self, ast: &ast::Ast, file: &mut File) -> Result<(), CompileError> {
        writeln!(file, "section .data")?;
        for decl in &ast.decls {
            match decl {
                ast::Decl::Var(var) => writeln!(file, "        {}: dq 0", var.name,)?,
                ast::Decl::Proc(_) => continue,
            }
        }
        Ok(())
    }

    fn compile_bss(&mut self, _ast: &ast::Ast, file: &mut File) -> Result<(), CompileError> {
        writeln!(file, "section .bss")?;
        writeln!(
            file,
            "        print_buf: resb 65; 64 for binary plus newline"
        )?;
        Ok(())
    }

    fn compile_global_variable_initialization(
        &mut self,
        var: &ast::Var,
        file: &mut File,
    ) -> Result<(), CompileError> {
        if let Some(expr) = &var.value {
            self.compile_assign(&var.name, expr, file)?;
        }
        Ok(())
    }

    fn compile_proc(&mut self, proc: &ast::Proc, file: &mut File) -> Result<(), CompileError> {
        writeln!(file, "{}:", proc.name)?;
        self.compile_block(&proc.body, file)?;
        writeln!(file, "        ret")?;
        Ok(())
    }

    fn compile_block(&mut self, block: &ast::Block, file: &mut File) -> Result<(), CompileError> {
        for stmt in &block.body {
            self.compile_stmt(stmt, file)?;
        }
        if let Some(ret_expr) = &block.ret_expr {
            self.compile_expr(ret_expr, file)?;
        }
        Ok(())
    }

    fn compile_expr(&mut self, expr: &ast::Expr, file: &mut File) -> Result<(), CompileError> {
        match expr {
            ast::Expr::IntLiteral(num) => {
                writeln!(file, "        mov rax, {}", num)?;
                Ok(())
            }
            ast::Expr::IntrinsicCall(name, exprs) => {
                self.compile_interinsic_call(name, exprs, file)
            }
            ast::Expr::Word(word) => {
                writeln!(file, "        mov rax, [{}]", word)?;
                Ok(())
            }
            ast::Expr::Op { left, right, op } => {
                self.compile_expr(right, file)?;
                // Push the result onto the stack. So that we can use it after evaluating the rhs.
                writeln!(file, "        push rax")?;
                self.compile_expr(left, file)?;
                // Now rax contains the result of the lhs.
                // We can pop our previously stored value into rbx
                writeln!(file, "        pop rbx")?;
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
                        self.write_comment(file,"mul multiplies rax by the arg and then stores the result in rax and rdx")?;
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
                    ast::Op::Gt => {
                        writeln!(file, "        mov rcx, 0")?;
                        writeln!(file, "        mov rdx, 1")?;
                        writeln!(file, "        cmp rax, rbx")?;
                        writeln!(file, "        cmovg rcx, rdx")?;
                        writeln!(file, "        mov rax, rcx")?;
                    }
                    ast::Op::Lt => {
                        writeln!(file, "        mov rcx, 0")?;
                        writeln!(file, "        mov rdx, 1")?;
                        writeln!(file, "        cmp rax, rbx")?;
                        writeln!(file, "        cmovl rcx, rdx")?;
                        writeln!(file, "        mov rax, rcx")?;
                    }
                }
                Ok(())
            }
            ast::Expr::BracketExpr(expr) => self.compile_expr(expr, file),
            ast::Expr::If { cond, then, else_ } => self.compile_if(file, cond, then, else_),
            ast::Expr::Block(block) => {
                self.write_comment(file, "Beginning block")?;
                for stmt in &block.body {
                    self.compile_stmt(stmt, file)?;
                }
                if let Some(ret_expr) = &block.ret_expr {
                    self.write_comment(file, "Block return expression.")?;
                    self.compile_expr(ret_expr, file)?;
                }
                Ok(())
            }
        }
    }

    fn compile_while(
        &mut self,
        file: &mut File,
        cond: &ast::Expr,
        body: &ast::Block,
    ) -> Result<(), CompileError> {
        let while_idx = self.next_jump_idx();
        writeln!(file, "JMP_WHILE_START_{while_idx}:")?;
        self.compile_expr(cond, file)?;
        writeln!(file, "        cmp rax, 0")?;
        writeln!(file, "        je JMP_WHILE_END_{while_idx}")?;
        self.compile_block(body, file)?;
        writeln!(file, "        jmp JMP_WHILE_START_{while_idx}")?;
        writeln!(file, "JMP_WHILE_END_{while_idx}:")?;
        Ok(())
    }

    fn compile_if(
        &mut self,
        file: &mut File,
        cond: &ast::Expr,
        then: &ast::Block,
        else_: &Option<Box<ast::Expr>>,
    ) -> Result<(), CompileError> {
        self.compile_expr(cond, file)?;
        let if_idx = self.next_jump_idx();

        // condition logic. Get the current if index and setup jumps based on the
        // result of the comparison.
        writeln!(file, "        cmp rax, 0")?;
        writeln!(file, "        jne JMP_IF_THEN_{if_idx}")?;
        if else_.is_some() {
            writeln!(file, "        jmp JMP_IF_ELSE_{if_idx}")?;
        } else {
            writeln!(file, "        jmp JMP_IF_END_{if_idx}")?;
        }

        // Then block
        writeln!(file, "JMP_IF_THEN_{if_idx}:")?;
        self.compile_block(then, file)?;
        writeln!(file, "        jmp JMP_IF_END_{if_idx}")?;

        // Else block
        if let Some(else_) = else_ {
            writeln!(file, "JMP_IF_ELSE_{if_idx}:")?;
            self.compile_expr(else_, file)?;
            writeln!(file, "        jmp JMP_IF_END_{if_idx}")?;
        }

        writeln!(file, "JMP_IF_END_{if_idx}:")?;
        Ok(())
    }

    fn write_comment(&self, file: &mut File, msg: &str) -> Result<(), CompileError> {
        // TODO: Line numbers. Pass trait into write comment what has file name and number.
        write!(file, "; ")?;
        writeln!(file, "{}", msg)?;
        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &ast::Stmt, file: &mut File) -> Result<(), CompileError> {
        match stmt {
            ast::Stmt::Expr(expr) => self.compile_expr(expr, file),
            ast::Stmt::While { cond, body } => self.compile_while(file, cond, body),
            ast::Stmt::Assign(name, expr) => self.compile_assign(name, expr, file),
        }
    }

    fn compile_assign(
        &mut self,
        name: &String,
        expr: &ast::Expr,
        file: &mut File,
    ) -> Result<(), CompileError> {
        self.compile_expr(expr, file)?;
        writeln!(file, "        mov qword [{}], rax", name)?;
        Ok(())
    }

    fn compile_interinsic_call(
        &mut self,
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

    fn next_jump_idx( &mut self,) -> usize{
        let jump_idx = self.n_jumps;
        self.n_jumps += 1;
        jump_idx
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
