use lexer::lexer::{TokenType, Token};
use std::convert::From;
use crate::DataType;


#[derive(Debug, Clone)]
pub enum Statement {
    VariableDeclaration(VarDecl),
    FunctionDefinition(FunctionDefinition),
    Assignment(Assign),
    Return(ReturnStmt),
    Expr(TypedExpr),
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub expr: Expr,
    pub ty: DataType,
}

impl TypedExpr {
    pub fn new(expr: Expr, ty: DataType) -> Self {
        Self {
            expr,
            ty,
        }
    }

    pub fn unknown(expr: Expr) -> Self {
        Self {
            expr,
            ty: DataType::Unknown,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<TypedExpr>
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub fn_name: Token,
    pub ret_type: DataType,
    pub fn_arguments: Vec<(String, DataType)>,
    pub body: Vec<Statement>
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub value: TypedExpr,
    pub data_type: Option<DataType>,
}


#[derive(Debug, Clone)]
pub struct Assign {
    pub name: String,
    pub value: TypedExpr,
}

// TODO: Store token for 
// Accessing lexeme, line, col
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Unary {
        op: UnaryOp, 
        right: Box<TypedExpr>,
    },
    Binary {
        left: Box<TypedExpr>,
        op: BinaryOp,
        right: Box<TypedExpr>
    },
    Grouping(Box<TypedExpr>),

    Variable {
        name: String,
        tok: Token,
    },

    FunctionCall {
        // Temp. For prototyping
        name: String,
        callee: Box<TypedExpr>,
        args:   Vec<TypedExpr>,
    },

    If {
        condition: Box<TypedExpr>,
        if_block: Vec<Statement>,
        else_block: Option<Vec<Statement>>,
    },

    While {
        condition: Box<TypedExpr>,
        body: Vec<Statement>,
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    // TODO: Support more types, be specific
    Number(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    None,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Plus, Minus,
    Star, Slash, Mod,

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
            TokenType::Mod  => BinaryOp::Mod,
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

impl UnaryOp {
    pub fn as_str(self) -> &'static str {
        match self {
            UnaryOp::Bang => "Unary\n!",
            UnaryOp::Minus => "Unary\n-",
        }
    }

    pub fn as_literal(self) -> &'static str {
        match self {
            UnaryOp::Bang => "!",
            UnaryOp::Minus => "-",
        }
    }
}

impl BinaryOp {
    pub fn as_str(self) -> &'static str {
        match self {
            BinaryOp::Plus => "Binary\n+",
					  BinaryOp::Minus => "Binary\n-",
            BinaryOp::Star => "Binary\n*",
					  BinaryOp::Slash => "Binary\n/",
            BinaryOp::Mod => "Binary\n%",

            BinaryOp::EqualEqual => "Binary\n==",
					  BinaryOp::NotEqual => "Binary\n!=",
            BinaryOp::Greater  => "Binary\n>",
					  BinaryOp::GreaterEqual => "Binary\n>=",
            BinaryOp::Lesser => "Binary\n<",
					  BinaryOp::LesserEqual => "Binary\n<=",
        }
    }

    pub fn as_literal(self) -> &'static str {
        match self {
            BinaryOp::Plus => "+",
					  BinaryOp::Minus => "-",
            BinaryOp::Star => "*",
					  BinaryOp::Slash => "/",
            BinaryOp::Mod   => "%",

            BinaryOp::EqualEqual => "==",
					  BinaryOp::NotEqual => "!=",
            BinaryOp::Greater  => ">",
					  BinaryOp::GreaterEqual => ">=",
            BinaryOp::Lesser => "<",
					  BinaryOp::LesserEqual => "<=",
        }
    }
}

impl Literal {
    fn as_string(self) -> String {
        match self {
            Self::Number(no) => format!("Integer:\n {}", no),
            Self::Float(no) => format!("Float:\n {}", no),
            Self::String(str) => format!("String:\n {}", str),
            Self::None => "Literal:\nNone".to_string(),
            Self::Boolean(boolean) => format!("Boolean: {}", boolean),
        }
    }
}

impl Expr {
    fn as_str(self) -> &'static str {
        match self {
            Expr::Unary{op, right} =>           "Unary",
            Expr::Binary{left, op, right} =>    "Binary",
            Expr::Literal(_lit) =>              "Literal",
            Expr::Grouping(_expr) =>            "Grouping",
            Expr::Variable{..} =>               "Variable",
            Expr::FunctionCall{..} =>           "Function call",
            Expr::If{..} =>                     "if",
            Expr::While{..} =>                  "while",
        }
    }
}

#[derive(Debug, Clone)]
pub struct PrintStmt {
    pub expr: Expr,
}


//void dump_dot(Node* root) {
//    size_t index = root - node_pool;
//
//    for (size_t i=0; i<sizeof(root->children) / sizeof(root->children[0]); ++i) {
//        if (root->children[i] != NULL) {
//            size_t child_index = root->children[i] - node_pool;
//            printf("    Node%zu -> Node%zu [label=%c];\n", index, child_index, (char) i);
//            dump_dot(root->children[i]);
//        }
//    }
//}
pub mod ast_printer {
    use crate::ast::Expr;
    use std::path::PathBuf;
    use std::fs::File;
    use utils::{append_to_file, get_file_ptr};

    pub struct Printer {
        out_file: PathBuf,
        file_ptr: File,
        cur_idx: i32,
    }

    impl Printer {
        pub fn new(file: PathBuf) -> Self {
            match get_file_ptr(file.clone()) {
                Ok(f) => Self {
                        out_file: file,
                        file_ptr: f,
                        cur_idx: 0,
                },
                Err(e) => panic!("ERROR: While opening output file: {}", e),
            }
        }

        pub fn generate_dump_dot(&mut self, ast: Expr) {
            let dot_start = "digraph AST {\nrankdir=TB;\nnode [shape=box, style=rounded, fontname=\"Fira Code\"];\n";
            let dot_end = "}";
            if let Err(e) = append_to_file(&mut self.file_ptr, dot_start) {
                eprintln!("Error: Could not append to {:?} due to: {}", self.out_file, e);
            }

            dump_dot(self, ast, self.get_index());

            if let Err(e) = append_to_file(&mut self.file_ptr, dot_end) {
                eprintln!("Error: Could not append to {:?} due to: {}", self.out_file, e);
            }

        }

        fn get_index(&self) -> i32 {
            return self.cur_idx;
        }

        fn next_index(&mut self) -> i32 {
            self.cur_idx += 1;
            return self.cur_idx;
        }

        fn append_output(&mut self, index: i32, child_index: i32, value: &str) {
            let node_string = format!("    Node{} -> Node{} [color=blue, label=\"{}\"];\n", index, child_index, value);
            if let Err(e) = append_to_file(&mut self.file_ptr, node_string.as_str()) {
                eprintln!("Error: Could not append to {:?} due to: \"{}\"", self.out_file, e);
            }
        }
    }

    pub fn dump_dot(printer: &mut Printer, ast: Expr, parent: i32) {
        match ast {
            //Expr::Unary{op, right} => {
            //    let op_idx = printer.next_index();
            //    printer.append_output(parent, op_idx, op.as_str());
            //    dump_dot(printer, *right, op_idx);
            //}
            //Expr::Binary {left, op, right} => {
            //    let op_idx = printer.next_index();
            //    printer.append_output(parent, op_idx, op.as_str());
            //    dump_dot(printer, *left, op_idx);
            //    dump_dot(printer, *right, op_idx);
            //},
            //Expr::Literal(literal) => {
            //    let op_idx = printer.next_index();
            //    printer.append_output(parent, op_idx, &literal.as_string());
            //},
            //Expr::Grouping(expr) => {
            //    let grp_idx = printer.next_index();
            //    let group = (*expr).clone();
            //    let group_str = group.clone().as_str();
            //    printer.append_output(parent, grp_idx, group_str);
            //    dump_dot(printer, group, grp_idx);
            //},
            //
            //Expr::Variable{name, ..} => {
            //    let op_idx = printer.next_index();
            //    printer.append_output(parent, op_idx, &name);
            //}

            _ => unimplemented!(),
        }
    }
}
