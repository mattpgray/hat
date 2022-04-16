use std::env;
use std::fs;
use std::process;
use std::process::exit;
mod lexer;

//------------------------------ parser end --------------------------------

fn usage() {
    println!("The hat programming language");
    println!("Usage: hat SUBCOMMAND");
    println!("SUMCOMMANDS");
    println!("    help         print this usage information");
    println!("    lex <file>   print the lexing information for the provided file");
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
        "lex" => {
            if args.len() == 0 {
                eprintln!("No file provided");
                usage();
                process::exit(1);
            }
            let file_path = args.remove(0);
            let file_data = fs::read_to_string(file_path.clone()).unwrap();
            let mut lexer = lexer::Lexer::new(file_data.chars(), file_path);
            loop {
                let tok = lexer.next();
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
