use std::env;
use std::fs;
use std::process;
use std::process::exit;
use std::str::Chars;
mod ast;
mod lexer;

//------------------------------ parser end --------------------------------

fn usage() {
    println!("The hat programming language");
    println!("Usage: hat SUBCOMMAND");
    println!("SUMCOMMANDS");
    println!("    help         print this usage information");
    println!("    lex   <file>   print the lexing information for the provided file");
    println!("    parse <file>   print the ast for the provided file");
}

fn get_file_path_and_data(args: &mut Vec<String>) -> (String, String) {
    if args.len() == 0 {
        eprintln!("No file provided");
        usage();
        process::exit(1);
    }
    let file_path: String = args.remove(0);
    (file_path.clone(), fs::read_to_string(file_path).unwrap())
}

fn main() {
    let mut args: Vec<String> = env::args().collect();
    args.remove(0);
    if args.len() == 0 {
        eprintln!("No subcommand provided");
        usage();
        process::exit(1);
    }
    let subcommand: String = args.remove(0);
    match subcommand.as_str() {
        "ast" => {
            let (file_path, file_data) = get_file_path_and_data(&mut args);
            let mut l = lexer::Lexer::new(file_data.chars(), file_path);
            let ast = ast::AST::parse(&mut l).unwrap();
            println!("{:?}", ast);
        }
        "lex" => {
            let (file_path, file_data) = get_file_path_and_data(&mut args);
            let mut l = lexer::Lexer::new(file_data.chars(), file_path);
            loop {
                let tok = l.next();
                match tok.kind {
                    lexer::TokenKind::EOF => break,
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
