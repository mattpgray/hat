use std::fmt;
use std::iter::Peekable;

#[derive(Debug, Clone)]
pub struct Loc {
    filename: String,
    row: usize,
    col: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.filename, self.row, self.col)
    }
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

    // Literals
    IntLiteral(u64),
    Word,
    Keyword(Keyword),

    // Keywords

    // Terminators
    Invalid,
    EOF,
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
            TokenKind::IntLiteral(val) => write!(f, "int({})", val),
            TokenKind::Word => write!(f, "word"),
            TokenKind::Keyword(_) => write!(f, "keyword"),
            TokenKind::Invalid => write!(f, "invalid"),
            TokenKind::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Proc,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Keyword::Proc => write!(f, "proc"),
        }
    }
}

impl Keyword {
    fn from(s: &str) -> Option<Self> {
        match s {
            "proc" => Some(Keyword::Proc),
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

pub struct Lexer<Chars: Iterator<Item = char>> {
    chars: Peekable<Chars>,
    file_path: String,
    lnum: usize,
    cnum: usize,
    peeked: Option<Token>,
}

impl<Chars: Iterator<Item = char>> Lexer<Chars> {
    pub fn new(chars: Chars, file_path: String) -> Lexer<Chars> {
        Lexer {
            chars: chars.peekable(),
            peeked: None,
            file_path,
            lnum: 0,
            cnum: 0,
        }
    }

    pub fn loc(&self) -> Loc {
        Loc {
            filename: self.file_path.clone(),
            row: self.lnum + 1,
            col: self.cnum + 1,
        }
    }

    pub fn peek(&mut self) -> &Token {
        match self.peeked {
            Some(ref t) => t,
            None => {
                let t = self.next();
                self.peeked.insert(t)
            }
        }
    }

    pub fn next(&mut self) -> Token {
        self.peeked.take().unwrap_or_else(|| self.read_token())
    }

    fn trim_whitespace(&mut self) {
        while let Some(c) = self.chars.peek() {
            if c.is_whitespace() {
                match self.chars.next().unwrap() {
                    '\n' => {
                        self.lnum += 1;
                        self.cnum = 0;
                    }
                    _ => self.cnum += 1,
                }
            } else {
                break;
            }
        }
    }

    fn drop_line(&mut self) {
        while let Some(c) = self.chars.next() {
            match c {
                '\n' => {
                    self.lnum += 1;
                    self.cnum = 0;
                    break;
                }
                _ => self.cnum += 1,
            }
        }
    }

    // remove all whitespace and comments
    fn next_valid_char_and_loc(&mut self) -> Option<(char, Loc)> {
        while let Some(_) = self.chars.peek() {
            self.trim_whitespace();
            let loc = self.loc();
            match self.chars.next() {
                Some(c) => {
                    if c == '/' {
                        if let Some('/') = self.chars.peek() {
                            self.drop_line();
                            continue;
                        }
                    }
                    self.cnum += 1;
                    return Some((c, loc));
                }
                None => break,
            }
        }

        return None;
    }

    fn read_token(&mut self) -> Token {
        let char_and_loc = self.next_valid_char_and_loc();
        match char_and_loc {
            Some((c, loc)) => {
                self.cnum += 1;
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
                    '0'..='9' => {
                        let mut int_val = c as u64 - '0' as u64;
                        let mut text = c.to_string();
                        while let Some(c) = self.chars.peek() {
                            match c {
                                '0'..='9' => {
                                    int_val = int_val * 10 + (*c as u64 - '0' as u64);
                                    text.push(*c);
                                    self.chars.next();
                                    self.cnum += 1;
                                }
                                _ => break,
                            }
                        }
                        Token {
                            kind: TokenKind::IntLiteral(int_val),
                            loc,
                            text,
                        }
                    }
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let mut word = c.to_string();
                        while let Some(c) = self.chars.peek() {
                            match c {
                                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                                    word.push(*c);
                                    self.chars.next();
                                    self.cnum += 1;
                                }
                                _ => break,
                            }
                        }
                        match Keyword::from(&word) {
                            Some(kw) => Token {
                                kind: TokenKind::Keyword(kw),
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
            None => Token {
                kind: TokenKind::EOF,
                loc: self.loc(),
                text: String::new(),
            },
        }
    }
}
