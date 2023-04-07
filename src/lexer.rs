use std::fmt;
use std::fs;

#[derive(Debug, Clone)]
pub struct Loc {
    file_path: String,
    row: usize,
    col: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file_path, self.row, self.col)
    }
}

fn char_is_numeric(c: char) -> bool {
    // We are not supporting unicode, so do not pretend to.
    c >= '0' && c <= '9'
}

fn char_is_alphanumeric(c: char) -> bool {
    // We are not supporting unicode, so do not pretend to.
    char_is_numeric(c) || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

fn char_is_word(c: char) -> bool {
    char_is_alphanumeric(c) || c == '_'
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Special chars
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    SemiColon,
    Hash,
    Comma,

    // Operators
    Eq,
    Minus,
    Add,
    Mul,
    Div,
    RAngleBracket,
    LAngleBracket,

    // Literals
    IntLiteral,
    Word,

    // Keywords
    Proc,
    If,
    Else,
    Var,
    While,

    // Terminators
    Invalid,
    EndOfFile,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::OpenCurly => write!(f, "{{"),
            TokenKind::CloseCurly => write!(f, "}}"),
            TokenKind::OpenParen => write!(f, "("),
            TokenKind::CloseParen => write!(f, ")"),
            TokenKind::SemiColon => write!(f, ";"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Hash => write!(f, "#"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Add => write!(f, "+"),
            TokenKind::Mul => write!(f, "*"),
            TokenKind::Div => write!(f, "/"),
            TokenKind::RAngleBracket => write!(f, ">"),
            TokenKind::LAngleBracket => write!(f, "<"),
            TokenKind::IntLiteral => write!(f, "int literal"),
            TokenKind::Word => write!(f, "word"),
            TokenKind::Proc => write!(f, "proc"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Var => write!(f, "var"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::While => write!(f, "while"),
            TokenKind::Invalid => write!(f, "invalid"),
            TokenKind::EndOfFile => write!(f, "EOF"),
        }
    }
}

impl TokenKind {
    fn from_keyword(k: &str) -> Option<TokenKind> {
        match k {
            "proc" => Some(TokenKind::Proc),
            "if" => Some(TokenKind::If),
            "else" => Some(TokenKind::Else),
            "var" => Some(TokenKind::Var),
            "while" => Some(TokenKind::While),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
    pub text: String,
}

pub struct Lexer {
    file_chars: Vec<char>,
    cursor: usize,
    file_path: String,
    peeked: Option<Token>,
}

impl Lexer {
    pub fn from_file(file_path: String) -> Result<Lexer, std::io::Error> {
        let file_data = fs::read_to_string(&file_path)?;
        Ok(Lexer {
            file_chars: file_data.chars().collect(),
            cursor: 0,
            peeked: None,
            file_path,
        })
    }

    pub fn loc(&self) -> Loc {
        let mut row: usize = 1;
        let mut col: usize = 1;
        for i in 0..self.cursor {
            if self.file_chars[i] == '\n' {
                col = 1;
                row += 1;
            } else {
                col += 1;
            }
        }
        Loc {
            file_path: self.file_path.clone(),
            row,
            col,
        }
    }

    pub fn peek(&mut self) -> &Token {
        let tok = match self.peeked {
            Some(ref t) => t,
            None => {
                let t = self.read_token();
                self.peeked.insert(t)
            }
        };
        tok
    }

    pub fn next(&mut self) -> Token {
        self.peeked.take().unwrap_or_else(|| self.read_token())
    }

    fn trim_whitespace(&mut self) {
        while self.is_not_empty() && self.curr_char().is_whitespace() {
            self.cursor += 1;
        }
    }

    fn is_not_empty(&self) -> bool {
        self.cursor < self.file_chars.len()
    }

    fn is_empty(&self) -> bool {
        !self.is_not_empty()
    }

    fn curr_char(&self) -> char {
        self.file_chars[self.cursor]
    }

    fn chop_char(&mut self) -> char {
        let char = self.curr_char();
        self.cursor += 1;
        char
    }

    fn drop_line(&mut self) {
        while self.is_not_empty() && self.curr_char() != '\n' {
            self.cursor += 1;
        }
    }

    // remove all whitespace and comments
    fn trim_left(&mut self) {
        loop {
            // Trim all whitespace
            self.trim_whitespace();
            if self.is_empty() {
                break;
            }
            // If we have reached a char, we still trim if it is a comment.
            if self.curr_char() == '/'
                && self.cursor + 1 < self.file_chars.len()
                && self.file_chars[self.cursor + 1] == '/'
            {
                self.drop_line();
                continue;
            }
            // Otherwise we have reached a token.
            break;
        }
    }

    fn append_predicate<F: Fn(char) -> bool>(&mut self, buf: &mut String, predicate: F) {
        while self.is_not_empty() && predicate(self.curr_char()) {
            let c = self.chop_char();
            buf.push(c);
        }
    }

    fn read_token(&mut self) -> Token {
        self.trim_left();
        if self.is_empty() {
            return Token {
                kind: TokenKind::EndOfFile,
                loc: self.loc(),
                text: String::new(),
            };
        }
        let loc = self.loc();
        let c = self.chop_char();
        match c {
            '{' => Token {
                kind: TokenKind::OpenCurly,
                loc,
                text: c.to_string(),
            },
            '}' => Token {
                kind: TokenKind::CloseCurly,
                loc,
                text: c.to_string(),
            },
            '(' => Token {
                kind: TokenKind::OpenParen,
                loc,
                text: c.to_string(),
            },
            ')' => Token {
                kind: TokenKind::CloseParen,
                loc,
                text: c.to_string(),
            },
            ';' => Token {
                kind: TokenKind::SemiColon,
                loc,
                text: c.to_string(),
            },
            ',' => Token {
                kind: TokenKind::Comma,
                loc,
                text: c.to_string(),
            },
            '#' => Token {
                kind: TokenKind::Hash,
                loc,
                text: c.to_string(),
            },
            '=' => Token {
                kind: TokenKind::Eq,
                loc,
                text: c.to_string(),
            },
            '-' => Token {
                kind: TokenKind::Minus,
                loc,
                text: c.to_string(),
            },
            '+' => Token {
                kind: TokenKind::Add,
                loc,
                text: c.to_string(),
            },
            '*' => Token {
                kind: TokenKind::Mul,
                loc,
                text: c.to_string(),
            },
            '/' => Token {
                kind: TokenKind::Div,
                loc,
                text: c.to_string(),
            },
            '>' => Token {
                kind: TokenKind::RAngleBracket,
                loc,
                text: c.to_string(),
            },
            '<' => Token {
                kind: TokenKind::LAngleBracket,
                loc,
                text: c.to_string(),
            },
            '0'..='9' => {
                let mut text = c.to_string();
                self.append_predicate(&mut text, char_is_numeric);
                Token {
                    kind: TokenKind::IntLiteral,
                    loc,
                    text,
                }
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut word = c.to_string();
                self.append_predicate(&mut word, char_is_word);
                match TokenKind::from_keyword(&word) {
                    Some(kind) => Token {
                        kind,
                        loc,
                        text: word,
                    },
                    None => Token {
                        kind: TokenKind::Word,
                        loc,
                        text: word,
                    },
                }
            }
            _ => Token {
                kind: TokenKind::Invalid,
                loc,
                text: c.to_string(),
            },
        }
    }
}
