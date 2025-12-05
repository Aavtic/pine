use lexer::lexer::{Token, TokenType, Object};
use ast::ast;

use std::fmt;



#[derive(Debug)]
#[allow(unused)]
enum ParseError {
    ParseError(String, usize, usize),
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // TODO:
            // Add filename for better error messages
            ParseError::ParseError(error, line, col) => write!(f, "SyntaxError: {}\nAt {}:{}", error, line, col),
        }
    }
}
impl std::error::Error for ParseError{}


macro_rules! matches_token {
    ($obj:expr, $($variant:expr),+ $(,)?) => {{
        // Calls check(...) for each variant; if any true -> advance() and true
        if $( $obj.check($variant) )||+ {
            $obj.advance();
            true
        } else {
            false
        }
    }};
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> ast::Expr {
        return self.expression();
    }
}

// Production Rules
impl Parser {
    fn expression(&mut self) -> ast::Expr {
        return self.equality()
    }

    fn equality(&mut self) -> ast::Expr {
        let mut expr = self.comparison();

        while matches_token!(self, TokenType::BangEqual, TokenType::EqualEqual) {
            let operator = self.previous();
            let right = self.comparison();
            expr = ast::Expr::Binary{
                left: Box::new(expr),
                op: ast::BinaryOp::from(operator.token_type),
                right: Box::new(right),
            }
        }

        return expr;
    }

    fn comparison(&mut self) -> ast::Expr {
        let mut expr = self.term();

        while matches_token!(self, 
            TokenType::Greater, TokenType::GreaterEqual,
            TokenType::Lesser, TokenType::LesserEqual) 
        {
            let operator = self.previous();
            let right = self.term();
            expr = ast::Expr::Binary{
                left: Box::new(expr),
                op: ast::BinaryOp::from(operator.token_type),
                right: Box::new(right),
            };
        }

        return expr;
    }

    fn term(&mut self) -> ast::Expr {
        let mut expr = self.factor();

        while matches_token!(self, TokenType::Minus, TokenType::Plus) {
            let operator = self.previous();
            let right = self.factor();
            expr = ast::Expr::Binary{
                left: Box::new(expr),
                op: ast::BinaryOp::from(operator.token_type),
                right: Box::new(right),
            };
        }

        return expr;
    }

    fn factor(&mut self) -> ast::Expr {
        let mut expr = self.unary();

        while matches_token!(self, TokenType::Slash, TokenType::Star) {
            let operator = self.previous();
            let right = self.unary();
            expr = ast::Expr::Binary{
                left: Box::new(expr),
                op: ast::BinaryOp::from(operator.token_type),
                right: Box::new(right),
            };
        }

        return expr;
    }

    fn unary(&mut self) -> ast::Expr {
        if matches_token!(self, TokenType::Bang, TokenType::Minus) {
            let operator = self.previous();
            let right = self.unary();

            return ast::Expr::Unary {
                op: ast::UnaryOp::from(operator.token_type),
                right: Box::new(right),
            }
        }

        return self.primary()
    }

    fn primary(&mut self) -> ast::Expr {
        if matches_token!(self, TokenType::True) {
            return ast::Expr::Literal(ast::Literal::Boolean(true))
        }
        if matches_token!(self, TokenType::False) {
            return ast::Expr::Literal(ast::Literal::Boolean(false))
        }
        if matches_token!(self, TokenType::None) {
            return ast::Expr::Literal(ast::Literal::None)
        }

        if matches_token!(self, TokenType::Number) {
            let number = self.previous();
            let obj = number.object.unwrap();
            if let Object::Integer(num) = obj {
                return ast::Expr::Literal(ast::Literal::Number(num))
            } else {
                panic!("Expected Integer in Token got: {:?}", obj)
            }
        }

        if matches_token!(self, TokenType::String) {
            let string = self.previous();
            let obj = string.object.unwrap();
            if let Object::String(str) = obj {
                return ast::Expr::Literal(ast::Literal::String(str))
            } else {
                panic!("Expected Integer in Token got: {:?}", obj)
            }
        }

        if matches_token!(self, TokenType::OpenPara) {
            let expr = self.expression();
            self.consume(TokenType::ClosePara, "Expected ')' after expression").unwrap();

            return ast::Expr::Grouping(Box::new(expr));
        }

        panic!("Add error handling here lol");
    }

}

// Helper Functions
impl Parser {
    fn consume(&mut self, t: TokenType, msg: &str) -> Result<Token, ParseError > {
        if self.check(t) {
            return Ok(self.advance())
        }
        let line = self.peek().line;
        let col = self.peek().column;
        return Err(ParseError::ParseError(msg.to_string(), line, col))
    }

    fn advance(&mut self) -> Token {
        if !self.is_end() {
            self.current += 1;
        }

        return self.previous()
    }

    fn previous(&self) -> Token {
        return self.tokens.get(self.current - 1).unwrap().clone();
    }

    fn check(&self, t: TokenType) -> bool {
        if self.is_end() {
            false;
        }
        return self.peek().token_type == t;
    }

    fn peek(&self) -> &Token {
        // TODO:
        // Handle this better
        return self.tokens.get(self.current).unwrap()
    }

    fn is_end(&self) -> bool {
        // TODO: Add EOF Type
        return self.peek().token_type == TokenType::EOF;
    }

    fn _synchronize(&mut self) {
        self.advance();

        while !self.is_end() {
            if self.previous().token_type == TokenType::SemiColon {
                return;
            }

            if matches!(self.peek().token_type,
                TokenType::Include
                | TokenType::Let
                | TokenType::For
                | TokenType::While
                | TokenType::If
                | TokenType::Return
            ) {
                return
            }

            self.advance();
        }
    }
}
