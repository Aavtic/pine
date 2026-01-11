#[derive(Debug, Clone)]
#[derive(PartialEq)]
pub enum DataType {
    I32,
    I64,
    U32,
    U64,
    String,
    Void,
    Custom(String)
}

impl DataType {
    pub fn to_str(&self) -> &'static str {
        match self {
            DataType::U32 => "u32",
            DataType::U64 => "u32",
            DataType::I32 => "i32",
            DataType::I64 => "i32",
            DataType::String => "string",
            DataType::Void  => "void",
            DataType::Custom(_cust) => "custom",
        }
    }
}

impl DataType {
    pub fn from(lexeme: &str) -> DataType {
        match lexeme {
            "u32" => DataType::U32,
            "u64" => DataType::U64,
            "i32" => DataType::I32,
            "i64" => DataType::U64,
            _     => DataType::Custom(lexeme.to_string()),
        }
    }
}
