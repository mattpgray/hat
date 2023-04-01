use std::env;
use std::fmt;
use std::fs;
use std::process;
use std::process::exit;

mod ast;
mod com;
mod lexer;
mod sim;

//------------------------------ parser end --------------------------------

fn usage() {
    println!("The hat programming language");
    println!("Usage: hat SUBCOMMAND");
    println!("SUMCOMMANDS");
    println!("    help           print this usage information");
    println!("    lex   <file>   print the lexing information for the provided file");
    println!("    ast   <file>   print the ast for the provided file");
    println!("    sim   <file>   run the provided file interactively");
}

fn get_file_path_and_data(args: &mut Vec<String>) -> (String, String) {
    if args.is_empty() {
        eprintln!("No file provided");
        usage();
        process::exit(1);
    }
    let file_path: String = args.remove(0);
    (file_path.clone(), fs::read_to_string(file_path).unwrap())
}

enum SimulationError {
    SyntaxError(ast::SyntaxError),
    ExecutionError(sim::ExecutionError),
}

impl From<ast::SyntaxError> for SimulationError {
    fn from(err: ast::SyntaxError) -> Self {
        SimulationError::SyntaxError(err)
    }
}

impl From<sim::ExecutionError> for SimulationError {
    fn from(err: sim::ExecutionError) -> Self {
        SimulationError::ExecutionError(err)
    }
}

impl fmt::Display for SimulationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SimulationError::SyntaxError(e) => write!(f, "{}: syntax error: {}", e.loc(), e),
            SimulationError::ExecutionError(e) => write!(f, "execution error: {}", e),
        }
    }
}

fn simulate_program(
    lexer: &mut lexer::Lexer<impl Iterator<Item = char>>,
) -> Result<(), SimulationError> {
    let ast = ast::Ast::parse(lexer)?;
    let mut ctx = sim::Context::default();
    ctx.run(ast)?;
    Ok(())
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
                i = i + 1;
                out_file = args.get(i).unwrap().to_owned();
            }
            _ => {
                in_file = Some(args[i].to_string());
            }
        }
        i = i + 1;
    }
    let in_file = in_file.unwrap();
    CompileArgs { out_file, in_file }
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
            let file_data = fs::read_to_string(&compile_args.in_file).unwrap();
            let mut l = lexer::Lexer::new(file_data.chars(), compile_args.in_file);
            let ast = ast::Ast::parse(&mut l).unwrap_or_else(|err| {
                eprintln!("{}: syntax error: {}", err.loc(), err);
                exit(1);
            });
            let mut c = com::Compiler::default();
            c.compile(compile_args.out_file, &ast).unwrap_or_else(|err| {
                match err {
                    com::CompileError::CmdError(cmd_err) => eprintln!(
                        "Error running command {}: exit code {}, output: \n{}",
                        cmd_err.name, cmd_err.code, cmd_err.message
                    ),
                    com::CompileError::Io(io_err) => eprintln!("io error: {}", io_err),
                }
                exit(1);
            });
        }
        "ast" => {
            let (file_path, file_data) = get_file_path_and_data(&mut args);
            let mut l = lexer::Lexer::new(file_data.chars(), file_path);
            let ast = ast::Ast::parse(&mut l);
            println!("{ast:#?}");
        }
        "sim" => {
            let (file_path, file_data) = get_file_path_and_data(&mut args);
            let mut l = lexer::Lexer::new(file_data.chars(), file_path);
            if let Err(err) = simulate_program(&mut l) {
                eprintln!("{}", err);
                exit(1);
            }
        }
        "lex" => {
            let (file_path, file_data) = get_file_path_and_data(&mut args);
            let mut l = lexer::Lexer::new(file_data.chars(), file_path);
            loop {
                let tok = l.next();
                match tok.kind {
                    lexer::TokenKind::EndOfFile => break,
                    lexer::TokenKind::Invalid => {
                        eprintln!("{}: invalid token: {}", tok.loc, tok.text);
                        exit(1);
                    }
                    _ => {
                        println!("{}: {}, {}", tok.loc, tok.kind, tok.text);
                    }
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
