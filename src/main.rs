use std::env;
use std::fs;
use std::process;

//------------------------------ lexer start --------------------------------
#[derive(Clone)]
struct Position {
    filename: String,
    row: i32,
    col: i32,
}

fn position_string(pos: &Position) -> String {
    format!("{}:{}:{}", pos.filename, pos.row, pos.col)
}

#[derive(Clone)]
struct Token {
    position: Position,
    typ: TokenType,
    str_val: String,
    int_val: u64,
}

#[derive(Copy, Clone)]
enum TokenType {
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,
    SemiColon,
    Word,
    IntLiteral,
}

fn token_type_string(t: TokenType) -> String {
    match t {
        TokenType::OpenCurly => "{".to_string(),
        TokenType::CloseCurly => "}".to_string(),
        TokenType::OpenBracket => "(".to_string(),
        TokenType::CloseBracket => ")".to_string(),
        TokenType::SemiColon => ";".to_string(),
        TokenType::Word => "word".to_string(),
        TokenType::IntLiteral => "int literal".to_string(),
    }
}

fn is_word_char(c: char) -> bool {
    (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || c == '#'
        || c == '$'
        || c == '_'
        || is_digit(c)
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn lex_file(filename: &String) -> Vec<Token> {
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
        let mut str_val: String = "".to_string();
        let mut int_val: u64 = 0;
        let start = Position {
            filename: filename.to_string(),
            row: row,
            col: col,
        };
        let mut width: usize = 0;

        if curr.starts_with("//") {
            while idx < contents.len() {
                let curr = &contents[idx..];
                if curr.starts_with('\n') {
                    idx = idx + 1;
                    row = row + 1;
                    col = 1;
                    break;
                }
                idx = idx + 1;
            }
            continue;
        }

        // Match simple tokens
        if curr.starts_with(';') {
            width = 1;
            token_type = TokenType::SemiColon;
        } else if curr.starts_with('{') {
            width = 1;
            token_type = TokenType::OpenCurly;
        } else if curr.starts_with('}') {
            width = 1;
            token_type = TokenType::CloseCurly;
        } else if curr.starts_with('(') {
            width = 1;
            token_type = TokenType::OpenBracket;
        } else if curr.starts_with(')') {
            width = 1;
            token_type = TokenType::CloseBracket;
        } else {
            assert!(curr.len() > 0);
            let mut chars = curr.chars();
            let c = match chars.next() {
                None => {
                    eprintln!("{}: Unexpected end of file", position_string(&start));
                    process::exit(1);
                }
                Some(c) => c,
            };

            // ints first so that words can contain numbers after the first char
            if is_digit(c) {
                let mut word_width: usize = 1;
                while let Some(c) = chars.next() {
                    if !is_digit(c) {
                        break;
                    }
                    word_width = word_width + 1;
                }
                let int_str = curr[..word_width].to_string();
                // Parse as decimal
                for c in int_str.chars() {
                    int_val *= 10;
                    int_val += c as u64 - '0' as u64;
                }

                width = word_width;
                token_type = TokenType::IntLiteral;
            } else if is_word_char(c) {
                let mut word_width: usize = 1;
                while let Some(c) = chars.next() {
                    if !is_word_char(c) {
                        break;
                    }
                    word_width = word_width + 1;
                }
                str_val = curr[..word_width].to_string();
                width = word_width;
                token_type = TokenType::Word;
            } else {
                eprintln!("{}: Unexpected token {}", position_string(&start), c);
                process::exit(1);
            }
        }
        tokens.push(Token {
            position: start,
            typ: token_type,
            str_val: str_val,
            int_val: int_val,
        });
        idx = idx + width;
        col = col + width as i32;
    }
    return tokens;
}

struct Lexer {
    lexemes: Vec<Token>,
    i: usize,
}

impl Lexer {
    fn next(&mut self) -> Option<Token> {
        if self.i >= self.lexemes.len() {
            return None;
        }
        let token = self.lexemes[self.i].clone();
        self.i += 1;
        Some(token)
    }

    fn peek(&self) -> Option<Token> {
        if self.i >= self.lexemes.len() {
            return None;
        }
        Some(self.lexemes[self.i].clone())
    }

    fn curr(&self) -> Token {
        self.lexemes[self.i].clone()
    }
}

//------------------------------ lexer end --------------------------------

//------------------------------ parser start --------------------------------

struct AST {
    proc: ProcAST,
}

enum ExprKind {
    ProcCall,
}

struct ProcCallAST {
    name: String,
    args: Vec<StmtAST>,
}

struct IntLitAST {
    val: u64,
}

struct ExprAST {
    kind: ExprKind,
    proc_call: ProcCallAST,
}

enum StmtKind {
    IntLit,
}

struct StmtAST {
    kind: StmtKind,
    int_lit: IntLitAST,
}

struct ProcAST {
    start: Position,
    name: String,
    body: ExprAST,
}

fn expect_token(lexer: &mut Lexer) -> Token {
    let curr_token = lexer.curr();
    match lexer.next() {
        None => {
            eprintln!(
                "{}: Unexpected end of file",
                position_string(&curr_token.position)
            );
            process::exit(1);
        }
        Some(t) => t,
    }
}

fn expect_any_word(lexer: &mut Lexer) -> Token {
    let token = expect_token(lexer);
    if !matches!(token.typ, TokenType::Word) {
        eprintln!(
            "{}: Unexpected token: {}, expected word",
            position_string(&token.position),
            token_type_string(token.typ),
        );
        process::exit(1);
    }
    assert!(token.str_val == "", "Empty str_val for word");
    token
}

fn expect_keyword(lexer: &mut Lexer, keyword: String) -> Token {
    let token = expect_any_word(lexer);
    if token.str_val != keyword {
        eprintln!(
            "{}: Unexpected word: {}, expected {}",
            position_string(&token.position),
            token.str_val,
            keyword
        );
        process::exit(1);
    }
    token
}

fn parse_scope(lexer: &mut Lexer) -> ExprAST {
    unimplemented!();
}

fn parse_proc(lexer: &mut Lexer) -> ProcAST {
    let start_token = expect_keyword(lexer, "proc".to_string());
    ProcAST{
        start: start_token.position.clone(),
        name: expect_any_word(lexer).str_val,
        body: parse_scope(lexer),
    }
}

fn parse_file(filename: String) -> AST {
    let lexemes = lex_file(&filename);
    let mut lexer = Lexer {
        lexemes: lexemes,
        i: 0,
    };
    AST {
        proc: parse_proc(&mut lexer),
    }
}

//------------------------------ parser end --------------------------------

fn usage() {
    println!("The hat programming language");
    println!("Usage: hat SUBCOMMAND");
    println!("SUMCOMMANDS");
    println!("    help         print this usage information");
    println!("    lex <file>   print the lexing information for the provided file");
    println!("    parse <file> print the ast for the provided file");
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
        "parse" => {
            if args.len() == 0 {
                eprintln!("No file provided");
                usage();
                process::exit(1);
            }
            parse_file(args.remove(0).clone());
        },
        "lex" => {
            if args.len() == 0 {
                eprintln!("No file provided");
                usage();
                process::exit(1);
            }
            let lexemes = lex_file(&args.remove(0));
            for lexeme in lexemes.iter() {
                println!(
                    "{}: {}, {}, {}",
                    position_string(&lexeme.position),
                    token_type_string(lexeme.typ),
                    lexeme.str_val,
                    lexeme.int_val,
                );
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
