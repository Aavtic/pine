use lexer::lexer::TokenType;
use std::convert::From;

// TODO: Store token for 
// Accessing lexeme, line, col
#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Unary {
        op: UnaryOp, 
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>
    },
    Grouping(Box<Expr>),
}

#[derive(Debug)]
pub enum Literal {
    // TODO: Support more types, be specific
    // Handle better
    Number(usize),
    String(String),
    Boolean(bool),
    None,
}

#[derive(Debug)]
pub enum UnaryOp{
    Bang, Minus,
}

impl From<TokenType> for UnaryOp {
    fn from(item: TokenType) -> UnaryOp {
        match item {
            TokenType::Bang => UnaryOp::Bang,
            TokenType::Minus => UnaryOp::Minus,

            _ => panic!("UNREACHABLE: Token escaped parsing checks. Token: {:?}", item),
        }
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Plus, Minus,
    Star, Slash,

    EqualEqual, NotEqual,
    Greater, GreaterEqual,
    Lesser, LesserEqual,
}


impl From<TokenType> for BinaryOp {
    fn from(item: TokenType) -> BinaryOp {
        match item {
            TokenType::Plus => BinaryOp::Plus,
            TokenType::Minus => BinaryOp::Minus,
            TokenType::Star => BinaryOp::Star,
            TokenType::Slash => BinaryOp::Slash,
            TokenType::EqualEqual => BinaryOp::EqualEqual,
            TokenType::BangEqual => BinaryOp::NotEqual,
            TokenType::Greater => BinaryOp::Greater,
            TokenType::GreaterEqual => BinaryOp::GreaterEqual,
            TokenType::Lesser => BinaryOp::Lesser,
            TokenType::LesserEqual => BinaryOp::LesserEqual,

            _ => panic!("UNREACHABLE: Token escaped parsing checks. Token: {:?}", item),
        }
    }
}
