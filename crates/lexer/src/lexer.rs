use std::fmt;
use std::process;

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub object: Option<Object>,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    // Single Character tokens
    OpenPara,
    ClosePara,
    OpenCurly,
    CloseCurly,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
    Mod,
    SemiColon,
    Colon,
    Dot,

    // One or Two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Lesser,
    LesserEqual,

    RightArrow, // ->

    Or,
    And,
    BitwiseOr,
    BitwiseAnd,

    // Literals
    Identifier,
    String,
    Number,
    Float,

    // Keywords
    Fn,
    Include,
    True,
    False,
    None,
    Let,
    For,
    If,
    Else,
    While,
    Return,
    PrintStmt,

    EOF, // Marks the end of token stream
}

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    Integer(usize),
    Float(f64),
}

#[derive(Debug)]
enum LexError {
    SyntaxError(String, usize, usize),
    UnterminatedString(usize, usize),
}
impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // TODO:
            // Add filename for better error messages
            LexError::SyntaxError(error, line, col) => {
                write!(f, "SyntaxError: {}\nAt {}:{}", error, line, col)
            }
            LexError::UnterminatedString(line, col) => {
                write!(f, "Unterminated String at {}:{}", line, col)
            }
        }
    }
}
impl std::error::Error for LexError {}

struct Tokenizer {
    // marks the current line
    line: usize,
    // marks the current column
    col: usize,
    // marks the string index in the source
    current: usize,
    // marks the start of token (temp)
    start: usize,
    // collect found tokens
    tokens: Vec<Token>,
    // source program
    source: String,
}

impl Tokenizer {
    fn new(source: &str) -> Tokenizer {
        Tokenizer {
            line: 1,
            col: 1,
            current: 0,
            start: 0,
            tokens: Vec::new(),
            source: source.to_string(),
        }
    }
}

// Helper functions
impl Tokenizer {
    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current);
        self.current += 1;
        // Handle this better
        return c.unwrap();
    }

    fn raise_error(&self, error_type: LexError) {
        eprintln!("{}", error_type);
        process::exit(1);
    }

    fn is_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn peek(&self) -> char {
        if self.is_end() {
            return '\0';
        }
        // TODO: Handle better
        return self.source.chars().nth(self.current).unwrap();
    }

    fn check_current(&mut self, c: char) -> bool {
        if self.is_end() {
            return false;
        }
        if self.peek() != c {
            return false;
        }
        self.advance();
        return true;
    }

    fn trim_string(&self) -> String {
        // TODO:
        // Handle this better
        self.source
            .get(self.start + 1..self.current - 1)
            .unwrap()
            .to_string()
    }

    fn get_current_token(&self) -> String {
        self.source
            .get(self.start..self.current)
            .unwrap()
            .to_string()
    }

    fn get_number(&self) -> usize {
        self.source
            .get(self.start..self.current)
            .unwrap()
            .parse::<usize>()
            .unwrap()
    }

    fn get_float(&self) -> f64 {
        self.source
            .get(self.start..self.current)
            .unwrap()
            .parse::<f64>()
            .unwrap()
    }

    fn add_token_string(&mut self) {
        while self.peek() != '"' && !self.is_end() {
            self.advance();
        }

        if self.is_end() {
            self.raise_error(LexError::UnterminatedString(self.line, self.col))
        }

        // get the last "
        self.advance();

        let trimmed = self.trim_string();

        self.add_token(TokenType::String, Some(Object::String(trimmed)))
    }

    fn add_token_number(&mut self) {
        let mut found_decimal = false;
        while self.peek().is_numeric() || self.peek() == '.' {
            if self.peek() == '.' {
                if found_decimal {
                    break
                } else {
                    found_decimal = true
                }
            }
            self.advance();
        }

        if found_decimal {
            let float = self.get_float();
            self.add_token(TokenType::Float, Some(Object::Float(float)))
        } else {
            let number = self.get_number();
            self.add_token(TokenType::Number, Some(Object::Integer(number)));
        }
    }

    fn add_token_identifier(&mut self) {
        while !self.is_end() && (self.peek().is_alphanumeric() || self.peek() == '_') {
            self.advance();
        }

        let token = self.get_current_token();

        match token.as_str() {
            // Keywords
            "fn" => self.add_token(TokenType::Fn, None),
            "include" => self.add_token(TokenType::Include, None),
            "true" => self.add_token(TokenType::True, None),
            "false" => self.add_token(TokenType::False, None),
            "let" => self.add_token(TokenType::Let, None),
            "if" => self.add_token(TokenType::If, None),
            "else" => self.add_token(TokenType::Else, None),
            "for" => self.add_token(TokenType::For, None),
            "while" => self.add_token(TokenType::While, None),
            "return" => self.add_token(TokenType::Return, None),
            "print" => self.add_token(TokenType::PrintStmt, None),
            "None" =>  self.add_token(TokenType::None, None),

            // Identifiers
            _ => self.add_token(TokenType::Identifier, None),
        }
    }
}

// Tokenization functions
impl Tokenizer {
    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::OpenPara, None),
            ')' => self.add_token(TokenType::ClosePara, None),
            '{' => self.add_token(TokenType::OpenCurly, None),
            '}' => self.add_token(TokenType::CloseCurly, None),
            ',' => self.add_token(TokenType::Comma, None),
            '+' => self.add_token(TokenType::Plus, None),
            '-' => {
                if self.check_current('>') {
                    self.add_token(TokenType::RightArrow, None);
                } else {
                    self.add_token(TokenType::Minus, None)
                }
            },
            '*' => self.add_token(TokenType::Star, None),
            '/' => self.add_token(TokenType::Slash, None),
            '%' => self.add_token(TokenType::Mod, None),
            ';' => self.add_token(TokenType::SemiColon, None),
            ':' => self.add_token(TokenType::Colon, None),
            '.' => self.add_token(TokenType::Dot, None),

            ' ' => self.col += 1,
            '\r' => {}
            '\t' => self.col += 4,

            '\n' => {
                self.line += 1;
                self.col = 0
            }

            '!' => {
                if self.check_current('=') {
                    self.add_token(TokenType::BangEqual, None)
                } else {
                    self.add_token(TokenType::Bang, None)
                }
            }

            '>' => {
                if self.check_current('=') {
                    self.add_token(TokenType::GreaterEqual, None);
                } else {
                    self.add_token(TokenType::Greater, None);
                }
            }

            '<' => {
                if self.check_current('=') {
                    self.add_token(TokenType::LesserEqual, None);
                } else {
                    self.add_token(TokenType::Lesser, None);
                }
            }

            '=' => {
                if self.check_current('=') {
                    self.add_token(TokenType::EqualEqual, None);
                } else {
                    self.add_token(TokenType::Equal, None);
                }
            }

            '|' => {
                if self.check_current('|') {
                    self.add_token(TokenType::Or, None);
                } else {
                    self.add_token(TokenType::BitwiseOr, None);
                }
            }

            '&' => {
                if self.check_current('&') {
                    self.add_token(TokenType::And, None);
                } else {
                    self.add_token(TokenType::BitwiseAnd, None);
                }
            }

            '"' => self.add_token_string(),
            _ => {
                if c.is_numeric() {
                    self.add_token_number();
                } else if c.is_alphabetic() || c == '_' {
                    self.add_token_identifier();
                } else {
                    self.raise_error(LexError::SyntaxError(
                        format!("Unexpected Character {}", c),
                        self.line,
                        self.col,
                    ));
                }
            }
        }
    }

    fn add_token(&mut self, token_type: TokenType, object: Option<Object>) {
        // TODO:
        // Handle this better
        let lexeme = self
            .source
            .get(self.start..self.current)
            .unwrap()
            .to_string();
        let length = lexeme.len();
        let line = self.line;
        let column = self.col;
        self.tokens.push(Token {
            token_type,
            lexeme,
            object,
            line,
            column,
        });

        self.col = self.col + length;
    }
}

pub fn lex(source: &str) -> Vec<Token> {
    let mut tokenizer = Tokenizer::new(source);

    while !tokenizer.is_end() {
        tokenizer.start = tokenizer.current;
        tokenizer.scan_token();
    }
    tokenizer.add_token(TokenType::EOF, None);

    return tokenizer.tokens;
}
