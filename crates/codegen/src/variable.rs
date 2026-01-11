use inkwell::{types::BasicTypeEnum, values::PointerValue };

#[derive(Debug, Clone)]
pub struct Variable<'ctx> {
    pub datatype: ast::DataType,
    pub llvm_type: BasicTypeEnum<'ctx>,
    pub ptr: PointerValue<'ctx>
}
