pub mod lexer {
    use std::fmt;

    #[derive(Debug)]
    pub struct Token {
        pub token_type: TokenType,
        pub lexeme:     String,
        pub line:       usize,
        pub column:     usize,
    }

    #[derive(Debug)]
    pub enum TokenType {
        // Single Character tokens
        OpenPara,
        ClosePara,
        OpenCurly,
        CloseCurly,
        Comma,
        Plus,
        Minus,
        SemiColon,

        // Literals
        Identifier,
        String,
        Number,

        // Keywords
        Fn,
        Include
    }

    #[derive(Debug)]
    enum LexError {
        SyntaxError(String),
    }
    impl fmt::Display for LexError {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                LexError::SyntaxError(error) => write!(f, "SyntaxError: {}", error),
            }
        }
    }
    impl std::error::Error for LexError{}

    fn is_word_breaker(c: char) -> bool {
        matches!(c, '(' | ')' | '{' | '}' | ';') || c.is_whitespace()
    }

    fn get_word(program: &str, cur: usize) -> Option<(String, usize)>{
        let bytes = program.as_bytes();
        let mut i = cur;

        // escape white spaces
        while i < bytes.len() {
            if bytes[i].is_ascii_whitespace() {
                i += 1;
            } else if is_word_breaker(bytes[i] as char) {
                return Some((String::from(bytes[i] as char), i + 1));
            } else {
                break
            }
        }

        let start = i;
        while i < bytes.len() {
            let c = bytes[i] as char;
            if is_word_breaker(c) {
                break;
            }
            i += 1;
        }

        if start == i {
            return None
        }

        let word = &program[start..i];

        return Some((word.to_string(), i));
    }

    fn is_identifier(_a: &String) -> bool {
        // TODO: 
        return true;
    }

    fn is_string(_a: &String) -> bool {
        // TODO: 
        return true;
    }

    fn is_number(_a: &String) -> bool {
        // TODO: 
        return true;
    }


    fn get_token(word: String, line: usize, column: usize) -> Result<Token, LexError> {
        match word.as_str() {
            "(" => return Ok(Token{ token_type: TokenType::OpenPara, lexeme: word, line, column, }),
            ")" => return Ok(Token{ token_type: TokenType::ClosePara, lexeme: word, line, column, }),
            "{" => return Ok(Token{ token_type: TokenType::OpenCurly, lexeme: word, line, column, }),
            "}" => return Ok(Token{ token_type: TokenType::CloseCurly, lexeme: word, line, column, }),
            "," => return Ok(Token{ token_type: TokenType::Comma, lexeme: word, line, column, }),
            "+" => return Ok(Token{ token_type: TokenType::Plus, lexeme: word, line, column, }),
            "-" => return Ok(Token{ token_type: TokenType::Minus, lexeme: word, line, column, }),
            ";" => return Ok(Token{ token_type: TokenType::SemiColon, lexeme: word, line, column, }),
            "fn" => return Ok(Token{ token_type: TokenType::Fn, lexeme: word, line, column, }),
            "include" => return Ok(Token{ token_type: TokenType::Include, lexeme: word, line, column, }),
            _ => {}
        }

        if is_identifier(&word) {
            Ok(Token{ token_type: TokenType::Identifier, lexeme: word, line, column, })
        } else if is_string(&word) {
            Ok(Token{ token_type: TokenType::String, lexeme: word, line, column, })
        } else if is_number(&word) {
            Ok(Token{ token_type: TokenType::Number, lexeme: word, line, column, })
        } else {
            Err(LexError::SyntaxError(format!("Unknown Token: {}", word)))
        }
    }

    pub fn lex(program: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        let lines = program.split("\n");
        for (line_no, line) in lines.enumerate() {
            let mut col:usize = 0;
            while let Some((word, col_n)) = get_word(line, col) {
                col = col_n;
                match get_token(word, line_no, col) {
                    Ok(token) => {tokens.push(token)
                    }
                    Err(e) => {
                        panic!("ERROR: {}", e);
                    }
                }
            }
        }
        return tokens;
    }
}
