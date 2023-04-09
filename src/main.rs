// I do not like code golf
#![allow(clippy::bool_to_int_with_if)]

use std::env;
use std::process;
use std::process::exit;

use lexer::SyntaxError;

mod ast;
mod com;
mod lexer;
mod sim;
mod types;

fn usage() {
    println!("The hat programming language");
    println!("Usage: hat SUBCOMMAND");
    println!("SUMCOMMANDS");
    println!("    help           print this usage information");
    println!("    lex   <file>   print the lexing information for the provided file");
    println!("    ast   <file>   print the ast for the provided file");
    println!("    sim   <file>   run the provided file interactively");
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
                in_file = Some(args[i].to_string());
            }
        }
        i += 1;
    }
    let in_file = in_file.unwrap();
    CompileArgs { out_file, in_file }
}

fn handle_simulation_err(err: sim::SimulationError) -> ! {
    match err {
        sim::SimulationError::ExecutionError(err) => eprintln!("execution error: {}", err),
        sim::SimulationError::AstError(err) => handle_ast_err(err),
    }
    exit(1);
}

fn handle_type_err(err: types::Error) -> ! {
    let msg = match err {
        types::Error::InitializationCycle(word) => {
            format!("initialization cycle detected at {word}")
        }
        types::Error::UnresolvedReference(word) => format!("unresolved reference {word}"),
        types::Error::UnsupportedOp { typ, op } => {
            format!("operations {op} is not supported for typ {typ}")
        }
        types::Error::Mismatch { left, right } => {
            format!("type mismatch, left = {left}, right = {right}")
        }
        types::Error::UntypedVariable(var) => format!("variable declared without a type {var}"),
        types::Error::UnknownType(typ) => format!("unknown type {typ}"),
        types::Error::UnexpectedNumberOfTypes { want, got } => {
            format!("expected {want} types but found {got}")
        }
        types::Error::NoMainFunction => "no main function".to_string(),

    };
    eprintln!("type error: {msg}");
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
        "sim" => {
            let mut ctx = sim::Context::default();
            ctx.run(get_file_path(&mut args))
                .unwrap_or_else(|err| handle_simulation_err(err));
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
