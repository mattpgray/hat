// I do not like code golf
#![allow(clippy::bool_to_int_with_if)]

use std::env;
use std::process;
use std::process::exit;

use lexer::SyntaxError;

mod ast;
mod com;
mod lexer;
mod types;

fn usage() {
    println!("The hat programming language");
    println!("Usage: hat SUBCOMMAND");
    println!("SUMCOMMANDS");
    println!("    help                   print this usage information");
    println!("    lex   <file>           print the lexing information for the provided file");
    println!("    ast   <file>           print the ast for the provided file");
    println!("    com   <file> [-o file] compile the provided file");
}
fn get_file_path(args: &mut Vec<String>) -> String {
    if args.is_empty() {
        eprintln!("No file provided");
        usage();
        process::exit(1);
    }
    args.remove(0)
}

struct CompileArgs {
    out_file: String,
    in_file: String,
}

fn parse_compile_args(args: Vec<String>) -> CompileArgs {
    let mut out_file = "out".to_string();
    let mut in_file: Option<String> = None;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                i += 1;
                out_file = args.get(i).unwrap().to_owned();
            }
            _ => {
                if in_file.is_some() {
                    eprintln!("compilation mode only supports one input file");
                    exit(1);
                }
                in_file = Some(args[i].to_string());
            }
        }
        i += 1;
    }
    let in_file = in_file.unwrap();
    CompileArgs { out_file, in_file }
}

fn handle_type_err(err: types::Error) -> ! {
    let (loc, msg) = match err {
        types::Error::InitializationCycle(loc, word) => (
            Some(loc),
            format!("initialization cycle detected at {word}"),
        ),
        types::Error::UnresolvedReference(loc, word) => {
            (Some(loc), format!("unresolved reference {word}"))
        }
        types::Error::UnsupportedOp { loc, typ, op } => (
            Some(loc),
            format!("operations {op} is not supported for typ {typ}"),
        ),
        types::Error::Mismatch { loc, left, right } => (
            Some(loc),
            format!("type mismatch, left = {left}, right = {right}"),
        ),
        types::Error::UntypedVariable(loc, var) => {
            (Some(loc), format!("variable declared without a type {var}"))
        }
        types::Error::UnknownType(loc, typ) => (Some(loc), format!("unknown type {typ}")),
        types::Error::UnexpectedNumberOfTypes { loc, want, got } => {
            (Some(loc), format!("expected {want} types but found {got}"))
        }
        types::Error::DuplicateReference{curr, prev, word} => {
            (Some(curr), format!("duplicate reference {word}. Previously defined at {prev}"))
        }
        types::Error::NoMainFunction => (None, "no main function".to_string()),
    };
    if let Some(loc) = loc {
        eprintln!("{loc}: type error: {msg}");
    } else {
        eprintln!("type error: {msg}");
    }
    exit(1);
}

fn handle_compile_err(err: com::CompileError) -> ! {
    match err {
        com::CompileError::CmdError(cmd_err) => eprintln!(
            "Error running command {}: exit code {}, output: \n{}",
            cmd_err.name, cmd_err.code, cmd_err.message
        ),
        com::CompileError::Io(io_err) => eprintln!("io error: {}", io_err),
        com::CompileError::AstError(err) => handle_ast_err(err),
        com::CompileError::TypeError(err) => handle_type_err(err),
    }
    exit(1);
}

fn handle_syntax_err(err: SyntaxError) -> ! {
    eprintln!("{}: syntax error: {}", err.loc(), err);
    exit(1);
}

fn handle_ast_err(err: ast::ASTError) -> ! {
    match err {
        ast::ASTError::SyntaxError(err) => handle_syntax_err(err),
        ast::ASTError::InvalidFile(file_path, err) => {
            eprintln!("could not open file {file_path}, {err}")
        }
    }
    exit(1);
}

fn main() {
    let mut args: Vec<String> = env::args().collect();
    args.remove(0);
    if args.is_empty() {
        eprintln!("No subcommand provided");
        usage();
        process::exit(1);
    }
    let subcommand: String = args.remove(0);
    match subcommand.as_str() {
        "com" => {
            let compile_args = parse_compile_args(args);
            let mut c = com::Compiler::default();
            c.compile(compile_args.in_file, compile_args.out_file)
                .unwrap_or_else(|err| handle_compile_err(err));
        }
        "ast" => {
            let ast = ast::Ast::from_file(get_file_path(&mut args))
                .unwrap_or_else(|err| handle_ast_err(err));
            println!("{ast:#?}");
        }
        "lex" => {
            let file_path = get_file_path(&mut args);
            let mut l = lexer::Lexer::from_file(file_path.clone()).unwrap_or_else(|err| {
                eprintln!("could not open file {file_path}, {err}");
                exit(1);
            });
            loop {
                match l.next() {
                    Ok(tok) => match tok.kind {
                        lexer::TokenKind::EndOfFile => break,
                        _ => {
                            println!("{}: {}, {}", tok.loc, tok.kind, tok.text);
                        }
                    },
                    Err(err) => handle_syntax_err(err),
                }
            }
        }
        "help" => {
            usage();
            process::exit(0);
        }
        _ => {
            eprintln!("Unknown subcommand {}", subcommand);
            usage();
            process::exit(1);
        }
    }
}
