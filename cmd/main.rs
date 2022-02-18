use std::env;
use std::process;
use std::fs;

#[derive(Clone)]
struct Position {
    filename: String,
    row: i32,
    col: i32,
}

fn position_string(pos: &Position) -> String {
    format!("{}:{}:{}", pos.filename, pos.row, pos.col)
}

struct Token {
    position: Position,
    typ: TokenType,
}

#[derive(Copy, Clone)]
enum TokenType {
    OpenCurly,
    CloseCurly,
}

fn lex_file(filename: &String) -> Vec<Token>{
    let contents = match fs::read_to_string(filename) {
        Err(e) => {
            eprintln!("Failed to read file {}: {}", filename, e);
            process::exit(1);
        }
        Ok(v) => v,
    };

    let mut tokens: Vec<Token> = Vec::new();
    let mut row: i32 = 1;
    let mut col: i32 = 1;
    let mut idx: usize = 0;
    while idx < contents.len() {
        while idx < contents.len() {
            let curr = &contents[idx..];
            if curr.starts_with(' ') {
                idx = idx + 1;
                col = col + 1;
            } else if curr.starts_with('\n') {
                idx = idx + 1;
                row = row + 1;
                col = 1;
            } else {
                break;
            }
        }
        if idx >= contents.len() {
            break;
        }
        let curr = &contents[idx..];
        let mut token_type: TokenType = TokenType::OpenCurly;
        let start = Position {
            filename: filename.to_string(),
            row: row,
            col: col,
        };
        println!("{}: {:?}", position_string(&start), &curr.chars().nth(0));
        let mut width: i32 = 0;
        if curr.starts_with('{') {
            width = 1;
            token_type = TokenType::OpenCurly;
        } else if curr.starts_with('}') {
            width = 1;
            token_type = TokenType::CloseCurly;
        } else {
            assert!(curr.len() > 0);
            let char = match curr.chars().next() {
                None => {
                    eprintln!("{}: Unexpected end of file", position_string(&start));
                    process::exit(1);
                },
                Some(c) => {
                    eprintln!("{}: Unexpected token {}", position_string(&start), c);
                    process::exit(1);
                },
            };
        }
        tokens.push(Token {
            position: start,
            typ: token_type,
        });
        idx = idx + width as usize;
        col = col + width;
    }
    return tokens;
}

fn usage() {
    println!("The hat programming language");
    println!("Usage: hat SUBCOMMAND");
    println!("SUMCOMMANDS");
    println!("    help       print this usage information");
    println!("    lex <file> print the lexing information for the provided file");
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
            lex_file(&args.remove(0));
        },
        "help" => {
            usage();
            process::exit(0);
        },
        _ => {
            eprintln!("Unknown subcommand {}", subcommand);
            usage();
            process::exit(1);
        }
    }
}
