#[derive(Debug, Clone)]
pub struct Function<'ctx> {
    pub name: String,
    pub datatype: ast::DataType,
    pub value: inkwell::values::FunctionValue<'ctx>,
    pub arguments: Vec<ast::DataType>
}
