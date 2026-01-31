#[derive(Debug, Clone)]
#[derive(PartialEq)]
pub enum DataType {
    I32,
    I64,
    U32,
    U64,
    F32,
    F64,
    String,
    Void,
    Unit,
    Boolean,
    Function {params: Vec<DataType>, ret_type: Box<DataType>},
    // For passing ast without type check
    Unknown,
}

impl DataType {
    pub fn to_str(&self) -> &'static str {
        match self {
            DataType::U32 => "u32",
            DataType::U64 => "u32",
            DataType::I32 => "i32",
            DataType::I64 => "i32",
            DataType::F32 => "f32",
            DataType::F64 => "f32",
            DataType::String => "string",
            DataType::Void  => "void",
            DataType::Boolean => "boolean",
            DataType::Function{..} => "function",
            DataType::Unknown => "unknown",
            DataType::Unit => "unit",
        }
    }
}

impl DataType {
    pub fn from(lexeme: &str) -> DataType {
        match lexeme {
            "u32" => DataType::U32,
            "u64" => DataType::U64,
            "i32" => DataType::I32,
            "i64" => DataType::I64,
            "f32" => DataType::F32,
            "f64" => DataType::F64,
            "bool" => DataType::Boolean,
            _     => {println!("[INFO]: Creating unknown type");
                DataType::Unknown
            },
        }
    }
}

impl DataType {
    pub fn unify(&self, other: &DataType) -> Result<DataType, String> {
        match (self, other) {
            // Same types can unify
            (DataType::I32, DataType::I32) => Ok(DataType::I32),
            (DataType::I64, DataType::I64) => Ok(DataType::I64),
            (DataType::U64, DataType::U64) => Ok(DataType::U64),
            (DataType::I64, DataType::I32) => Ok(DataType::I64),
            (DataType::U64, DataType::U32) => Ok(DataType::U64),
            (DataType::F64, DataType::F32) => Ok(DataType::F64),
            (DataType::F32, DataType::F32) => Ok(DataType::F32),
            (DataType::F64, DataType::F64) => Ok(DataType::F64),
            (DataType::Boolean, DataType::Boolean) => Ok(DataType::Boolean),
            (DataType::Void, DataType::Void) => Ok(DataType::Void),

            // Unknown types can unify with anything

            (DataType::Unknown, t) => Ok(t.clone()),

            // This should be possible too r 
            (t, DataType::Unknown) => Ok(t.clone()),

            (
                DataType::Function {
                    params: p1, ret_type: r1,
            },
                DataType::Function {
                    params: p2, ret_type: r2,
                }
            ) => {
                let params: Result<Vec<_>, _> =
                    p1.iter().zip(p2.iter()).map(|(a, b)| a.unify(b)).collect();
                let ret = r1.unify(r2)?;
                Ok(DataType::Function {
                    params: params?,
                    ret_type: Box::new(ret),
                })
            }

            // Type mismatch
            _ => Err(format!(
                "Type mismatch: expected {}, got {}",
                self.to_str(), other.to_str()
            )),
        }
    }
}
