use ast::DataType;
//use ast::ast;
use lexer::lexer::{Object, Token, TokenType};

use std::fmt;
use std::path::PathBuf;

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum ParseError {
    ParseError(String, usize, usize),
    UnexpectedToken(String, usize, usize),
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            // TODO:
            // Add filename for better error messages
            ParseError::ParseError(error, line, col) => {
                write!(f, "SyntaxError: {}\nAt {}:{}", error, line, col)
            }

            ParseError::UnexpectedToken(error, line, col) => {
                write!(f, "ParseError: {}\nAt {}:{}", error, line, col)
            }
        }
    }
}
impl std::error::Error for ParseError {}

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
    errors: Vec<ParseError>,
    current: usize,
    contains_parse_error: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            errors: Vec::new(),
            current: 0,
            contains_parse_error: false,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<ast::Statement>, ParseError> {
        let mut statements = Vec::new();
        while !self.is_end() {
            match self.statement() {
                Some(statement) => statements.push(statement),
                None => {}
            }
        }
        Ok(statements)
    }

    pub fn dump_ast(&self, ast: Vec<ast::Statement>, out_file: PathBuf) {
        todo!();
        //let mut printer = ast::ast_printer::Printer::new(out_file);
        // TODO:
        // Handle Error handling
        //
        //    for statement in ast {
        //        match statement {
        //            ast::Statements::(stmt) => {
        //                    printer.generate_dump_dot(expr),
        //            },
        //            _ => unimplemented!(),
        //        }
        //    }
    }
}

// Production Rul
impl Parser {
    fn statement(&mut self) -> Option<ast::Statement> {
        if matches_token!(self, TokenType::Let) {
            match self.var_declaration() {
                Ok(vardecl) => return Some(ast::Statement::VariableDeclaration(vardecl)),
                Err(err) => {
                    self._report_error(err);
                    self.print_current_error();
                    self.synchronize();
                    return None;
                }
            }
        }

        if matches_token!(self, TokenType::Fn) {
            match self.function_definition() {
                Ok(func_def) => return Some(ast::Statement::FunctionDefinition(func_def)),
                Err(err) => {
                    self._report_error(err);
                    self.print_current_error();
                    self.synchronize();
                    return None;
                }
            }
        }

        if matches_token!(self, TokenType::Return) {
            match self.return_statement() {
                Ok(ret_stmt) => return Some(ast::Statement::Return(ret_stmt)),
                Err(err) => {
                    self._report_error(err);
                    self.print_current_error();
                    self.synchronize();
                    return None;
                }
            }
        }

        if matches_token!(self, TokenType::Identifier) {
            // Check if equal is present so that we can confirm assignment statement
            if self.check(TokenType::Equal) {
                // Get both the identifier back
                self.backup();
                match self.assignment_statement() {
                    Ok(assign) => return Some(ast::Statement::Assignment(assign)),
                    Err(err) => {
                        self._report_error(err);
                        self.print_current_error();
                        self.synchronize();
                        return None;
                    }
                }
            } else {
                // Go to the identifier token
                self.backup();
            }
        }

        if let Ok(expr) = self.expression() {
            return Some(ast::Statement::Expr(expr));
        }

        self._report_error(ParseError::UnexpectedToken(
            self.peek().lexeme.clone(),
            self.peek().line,
            self.peek().column,
        ));
        self.print_current_error();
        self.synchronize();

        None
    }

    fn function_definition(&mut self) -> Result<ast::FunctionDefinition, ParseError> {
        let fn_name = self.consume(TokenType::Identifier, "Expected Function Name after `fn`")?;
        self.consume(TokenType::OpenPara, "Expected `(` after function name")?;

        let mut arguments = Vec::new();

        while !self.is_end() && !self.check(TokenType::ClosePara) {
            // Confirm if previous has a comma or open bracket
            if self.previous_check(TokenType::OpenPara) || self.check(TokenType::Comma) {
                if self.check(TokenType::Comma) {
                    self.advance();
                }
            } else {
                return Err(ParseError::UnexpectedToken(
                    format!(
                        "Unexpected Token Found: `{}`",
                        self.peek().lexeme.to_string()
                    ),
                    self.peek().line,
                    self.peek().column,
                ));
            }

            let identifier = self.consume(
                TokenType::Identifier,
                format!(
                    "Parameter identifier expected, Found `{}`",
                    self.peek().lexeme
                )
                .as_str(),
            )?;
            let mut data_type = DataType::Unknown;
            if self.check(TokenType::Colon) {
                self.advance();
                let datatype_lexeme = self
                    .consume(
                        TokenType::Identifier,
                        format!("Expected Type, Found `{}`", self.peek().lexeme).as_str(),
                    )?
                    .lexeme;
                data_type = DataType::from(&datatype_lexeme);
            }
            arguments.push((identifier.lexeme.to_string(), data_type));
        }

        self.consume(
            TokenType::ClosePara,
            format!("Expected Closing para `)` found `{}`", self.peek().lexeme).as_str(),
        )?;

        // Now Check if Function return type is defined
        let mut return_type = DataType::Unknown;

        if self.check(TokenType::RightArrow) {
            self.advance();
            let return_type_tok = self.consume(
                TokenType::Identifier,
                format!(
                    "Expected Function Return type after `->`, found: {}",
                    self.peek().lexeme
                )
                .as_str(),
            )?;
            return_type = DataType::from(&return_type_tok.lexeme);

            if fn_name.lexeme == "main" {
                if return_type != DataType::I32 {
                    return Err(ParseError::ParseError(
                        format!(
                            "main function should only return i32, found {}",
                            return_type.to_str()
                        ),
                        return_type_tok.line,
                        return_type_tok.column,
                    ));
                }
            }
        }

        // Main function by default returns i32
        // Todo: move this to analyzer
        if fn_name.lexeme == "main" {
            return_type = DataType::I32;
        }

        let body = self.block_expression()?;

        return Ok(ast::FunctionDefinition {
            fn_name: fn_name,
            ret_type: return_type,
            fn_arguments: arguments,
            body,
        });
    }

    fn return_statement(&mut self) -> Result<ast::ReturnStmt, ParseError> {
        let expr = self.expression().map(|x| Some(x)).unwrap_or_else(|_| None);

        if self.check(TokenType::SemiColon) {
            self.advance();
        }

        return Ok(ast::ReturnStmt { value: expr });
    }

    fn var_declaration(&mut self) -> Result<ast::VarDecl, ParseError> {
        let name = self.consume(TokenType::Identifier, "Expected Variable name")?;
        let mut data_type = None;

        if self.check(TokenType::Colon) {
            self.advance();
            let datatype_lexeme = self
                .consume(
                    TokenType::Identifier,
                    format!("Expected Type after `:`, found: `{}`", self.peek().lexeme).as_str(),
                )?
                .lexeme;
            data_type = Some(DataType::from(&datatype_lexeme));
        }

        self.consume(
            TokenType::Equal,
            "Expected `=`, and provide initializing value",
        )?;

        let initializer = self.expression()?;

        if self.check(TokenType::SemiColon) {
            self.advance();
        }

        Ok(ast::VarDecl {
            name: name.lexeme,
            // We need value
            value: initializer,
            data_type,
        })
    }

    fn assignment_statement(&mut self) -> Result<ast::Assign, ParseError> {
        let name = self.consume(TokenType::Identifier, "Expected Variable name")?;
        let _ = self.consume(
            TokenType::Equal,
            "Expected `=`, and provide initializing value",
        );

        let value = self.expression()?;

        if self.check(TokenType::SemiColon) {
            self.advance();
        }

        Ok(ast::Assign {
            name: name.lexeme,
            value,
        })
    }

    fn expression(&mut self) -> Result<ast::TypedExpr, ParseError> {
        return Ok(ast::TypedExpr::unknown(self.while_expression()?));
    }

    fn while_expression(&mut self) -> Result<ast::Expr, ParseError> {
        if matches_token!(self, TokenType::While) {
            let condition = self.expression()?;
            let body = self.block_expression()?;
            return Ok(ast::Expr::While{
                condition: Box::new(condition),
                body,
            })
        } else {
            return self.if_expression();
        }
    }

    fn if_expression(&mut self) -> Result<ast::Expr, ParseError> {
        if matches_token!(self, TokenType::If) {
            let condition = self.expression()?;
            let if_block = self.block_expression()?;
            let else_block = if matches_token!(self, TokenType::Else) {
                if self.check(TokenType::If) {
                    Some(vec![ast::Statement::Expr(ast::TypedExpr::unknown(self.if_expression()?))])
                } else {
                    Some(self.block_expression()?)
                }
            } else {
                None
            };

            return Ok(ast::Expr::If {
                condition: Box::new(condition),
                if_block,
                else_block,
            });
        } else {
            return self.equality();
        }
    }

    fn block_expression(&mut self) -> Result<Vec<ast::Statement>, ParseError> {
        self.consume(
            TokenType::OpenCurly,
            format!("Expected `{{`, Found: `{}`", self.peek().lexeme).as_str(),
        )?;
        let mut statements = Vec::new();

        while !self.is_end() && !self.check(TokenType::CloseCurly) {
            statements.push(match self.statement() {
                Some(st) => st,
                None => return Err(self.throw()),
            });
        }

        self.consume(
            TokenType::CloseCurly,
            "Expected }} after function definition",
        )?;

        return Ok(statements);
    }

    fn equality(&mut self) -> Result<ast::Expr, ParseError> {
        let mut expr = self.comparison();

        if expr.is_err() {
            return expr;
        };

        while matches_token!(self, TokenType::BangEqual, TokenType::EqualEqual) {
            let operator = self.previous();
            let right = self.comparison();

            if right.is_err() {
                return right;
            };

            expr = Ok(ast::Expr::Binary {
                left: Box::new(ast::TypedExpr::unknown(expr.unwrap())),
                op: ast::BinaryOp::from(operator.token_type),
                right: Box::new(ast::TypedExpr::unknown(right.unwrap())),
            })
        }

        return expr;
    }

    fn comparison(&mut self) -> Result<ast::Expr, ParseError> {
        let mut expr = self.term();

        if expr.is_err() {
            return expr;
        };

        while matches_token!(
            self,
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Lesser,
            TokenType::LesserEqual
        ) {
            let operator = self.previous();
            let right = self.term();

            if right.is_err() {
                return right;
            };

            expr = Ok(ast::Expr::Binary {
                left: Box::new(ast::TypedExpr::unknown(expr.unwrap())),
                op: ast::BinaryOp::from(operator.token_type),
                right: Box::new(ast::TypedExpr::unknown(right.unwrap())),
            });
        }

        return expr;
    }

    fn term(&mut self) -> Result<ast::Expr, ParseError> {
        let mut expr = self.factor();

        if expr.is_err() {
            return expr;
        };

        while matches_token!(self, TokenType::Minus, TokenType::Plus) {
            let operator = self.previous();
            let right = self.factor();
            if right.is_err() {
                return right;
            };

            expr = Ok(ast::Expr::Binary {
                left: Box::new(ast::TypedExpr::unknown(expr.unwrap())),
                op: ast::BinaryOp::from(operator.token_type),
                right: Box::new(ast::TypedExpr::unknown(right.unwrap())),
            });
        }

        return expr;
    }

    fn factor(&mut self) -> Result<ast::Expr, ParseError> {
        let mut expr = self.unary();

        if expr.is_err() {
            return expr;
        };

        while matches_token!(self, TokenType::Slash, TokenType::Star, TokenType::Mod) {
            let operator = self.previous();
            let right = self.unary();

            if right.is_err() {
                return right;
            };

            expr = Ok(ast::Expr::Binary {
                left: Box::new(ast::TypedExpr::unknown(expr.unwrap())),
                op: ast::BinaryOp::from(operator.token_type),
                right: Box::new(ast::TypedExpr::unknown(right.unwrap())),
            });
        }

        return expr;
    }

    fn unary(&mut self) -> Result<ast::Expr, ParseError> {
        if matches_token!(self, TokenType::Bang, TokenType::Minus) {
            let operator = self.previous();
            let right = self.unary();

            if right.is_err() {
                return right;
            };

            return Ok(ast::Expr::Unary {
                op: ast::UnaryOp::from(operator.token_type),
                // This should be safe
                right: Box::new(ast::TypedExpr::unknown(right.unwrap())),
            });
        }

        return self.call();
    }

    fn call(&mut self) -> Result<ast::Expr, ParseError> {
        let mut expr = self.primary();

        if expr.is_err() {
            return expr;
        }

        let name = self.previous().lexeme;

        loop {
            if matches_token!(self, TokenType::OpenPara) {
                expr = self.finish_call(expr.unwrap(), name.clone());
            } else {
                break;
            }
        }

        return expr;
    }

    fn finish_call(&mut self, callee: ast::Expr, name: String) -> Result<ast::Expr, ParseError> {
        let mut arguments = Vec::new();

        if !self.check(TokenType::ClosePara) {
            loop {
                arguments.push(self.expression()?);

                if !self.check(TokenType::Comma) {
                    break;
                }
                self.advance(); // advance the comma
            }
        }

        self.consume(
            TokenType::ClosePara,
            "Expected closing bracket `)` after arguments",
        )?;

        return Ok(ast::Expr::FunctionCall {
            name,
            callee: Box::new(ast::TypedExpr::unknown(callee)),
            args: arguments,
        });
    }

    fn primary(&mut self) -> Result<ast::Expr, ParseError> {
        if matches_token!(self, TokenType::True) {
            return Ok(ast::Expr::Literal(ast::Literal::Boolean(true)));
        }
        if matches_token!(self, TokenType::False) {
            return Ok(ast::Expr::Literal(ast::Literal::Boolean(false)));
        }
        if matches_token!(self, TokenType::None) {
            return Ok(ast::Expr::Literal(ast::Literal::None));
        }

        if matches_token!(self, TokenType::Number) {
            let number = self.previous();
            let obj = number.object.unwrap();
            if let Object::Integer(num) = obj {
                return Ok(ast::Expr::Literal(ast::Literal::Number(num as i64)));
            } else {
                panic!("Expected Integer in Token got: {:?}", obj)
            }
        }

        if matches_token!(self, TokenType::String) {
            let string = self.previous();
            let obj = string.object.unwrap();
            if let Object::String(str) = obj {
                return Ok(ast::Expr::Literal(ast::Literal::String(str)));
            } else {
                panic!("Expected Integer in Token got: {:?}", obj)
            }
        }

        if matches_token!(self, TokenType::OpenPara) {
            let expr = self.expression();
            if expr.is_err() {
                return Err(expr.err().unwrap());
            }

            if let Err(e) = self.consume(TokenType::ClosePara, "Expected ')' after expression") {
                return Err(e);
            }

            return Ok(ast::Expr::Grouping(Box::new(ast::TypedExpr::unknown(
                expr.unwrap().expr,
            ))));
        }

        if matches_token!(self, TokenType::Identifier) {
            let tok = self.previous();
            return Ok(ast::Expr::Variable {
                name: tok.lexeme.clone(),
                tok: tok,
            });
        }

        // Report unexpected token
        let token = self.peek();

        Err(ParseError::UnexpectedToken(
            format!("Unexpected Token: {}", token.lexeme),
            token.line,
            token.column,
        ))
    }
}

// Helper Functions
impl Parser {
    fn print_current_error(&self) {
        if self.errors.len() == 0 {
            return;
        }
        let error = self.errors.iter().nth(self.errors.len() - 1).unwrap();
        println!("{}", error);
    }

    fn throw(&self) -> ParseError {
        //if self.errors.len() == 0 {
        //    return;
        //}
        self.errors
            .iter()
            .nth(self.errors.len() - 1)
            .unwrap()
            .clone()
    }

    fn _report_error(&mut self, error: ParseError) {
        if !self.contains_parse_error {
            self.contains_parse_error = true;
        }
        self.errors.push(error);
    }

    fn consume(&mut self, t: TokenType, msg: &str) -> Result<Token, ParseError> {
        if self.check(t) {
            return Ok(self.advance());
        }
        let line = self.peek().line;
        let col = self.peek().column;
        return Err(ParseError::ParseError(msg.to_string(), line, col));
    }

    fn advance(&mut self) -> Token {
        if !self.is_end() {
            self.current += 1;
        }

        return self.previous();
    }

    fn previous(&self) -> Token {
        return self.tokens.get(self.current - 1).unwrap().clone();
    }

    fn backup(&mut self) {
        if self.current > 0 {
            self.current -= 1;
        }
    }

    fn previous_check(&self, t: TokenType) -> bool {
        if self.is_end() {
            false;
        }
        return self.previous().token_type == t;
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
        return self.tokens.get(self.current).unwrap();
    }

    fn is_end(&self) -> bool {
        // TODO: Add EOF Type
        return self.peek().token_type == TokenType::EOF;
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_end() {
            if self.previous().token_type == TokenType::SemiColon {
                return;
            }

            if matches!(
                self.peek().token_type,
                TokenType::Include
                    | TokenType::Let
                    | TokenType::For
                    | TokenType::While
                    | TokenType::If
                    | TokenType::Return
            ) {
                return;
            }

            self.advance();
        }
    }
}
