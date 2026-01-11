use ast::Statement;
use ast::types::DataType;

pub struct Analyzer {}

impl Analyzer {
    fn analyze(ast: Vec<Statement>) -> Vec<Statement> {
        for statement in ast {
        }
        todo!();
    }
}

impl Analyzer {
    fn get_primitive_type(type_str: &str) -> DataType {
        match type_str {
            "i32" => DataType::I32,
            "i64" => DataType::I64,
            "u32" => DataType::U32,
            "u64" => DataType::U64,
            "string" => DataType::String,
            
            _ => DataType::Custom(type_str.to_string())
        }
    }
}

impl Analyzer {
    #[inline]
    pub fn is_unsigned_integer(typ: &DataType) -> bool {
        [DataType::U32, DataType::U64].contains(typ)
    }

    #[inline]
    pub fn is_integer(typ: &DataType) -> bool {
        [
            DataType::I32, DataType::I64,
            DataType::U32, DataType::U64
        ].contains(typ)
    }
}
