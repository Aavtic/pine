use std::collections::HashMap;

use crate::{function::Function, variable::Variable};

use analyzer::analyzer::Analyzer;
use ast::{DataType, Expr, Literal, Statement};

use inkwell::{
    IntPredicate,
    AddressSpace,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::IntType,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue,
    },
};

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    variables: HashMap<String, Variable<'ctx>>,
    functions: HashMap<String, Function<'ctx>>,
    //variables: HashMap<String, >
    current_fn: Option<Function<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn create_context() -> Context {
        inkwell::targets::Target::initialize_all(&inkwell::targets::InitializationConfig::default());
        inkwell::context::Context::create()
    }

    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        module.set_source_file_name(&format!("{}.alp", module_name));
        module.set_triple(&inkwell::targets::TargetMachine::get_default_triple());

        module
            .add_global_metadata(
                "ident",
                &context.metadata_node(&[context
                    .metadata_string(&format!(
                        "pine compiler version {}",
                        env!("CARGO_PKG_VERSION")
                    ))
                    .into()]),
            )
            .unwrap();

        let variables = HashMap::new();
        let functions = HashMap::new();
        let current_fn = None;

        Self {
            context,
            builder,
            module,
            variables,
            functions,
            current_fn,
        }
    }

    pub fn compile(&mut self, statements: &Vec<Statement>) -> Result<&Module<'ctx>, String> {
        // Phase 1 delcare all functions

        for statement in statements {
            if let Statement::FunctionDefinition(fndef) = statement {
                let fn_name = &fndef.fn_name.lexeme;
                let mut input: Vec<(String, DataType)> = Vec::new();
                let ret_type = fndef.ret_type.clone();

                for arg in &fndef.fn_arguments {
                    input.push((arg.0.clone(), arg.1.as_ref().unwrap().clone()))
                }

                self.declare_function(fn_name, input.as_slice(), &ret_type)
                    .unwrap();
            }
        }

        // Phase 2
        for statement in statements {
            if let Statement::FunctionDefinition(_) = statement {
                self.compile_statement(statement, None)?;

                let function = self.current_fn.as_ref().unwrap().value;
                let last_block = function.get_last_basic_block().unwrap();

                if last_block.get_terminator().is_none() {
                    self.builder.position_at_end(last_block);

                    match function.get_type().get_return_type() {
                        Some(ret_ty) => {
                            // Non-void function → implicit return value
                            let zero = ret_ty.const_zero();
                            self.builder.build_return(Some(&zero)).unwrap();
                        }
                        None => {
                            // Void function
                            self.builder.build_return(None).unwrap();
                        }
                    }
                }
            }
        }

        //let main_fn = self.functions.get("main").unwrap();
        //let verified = main_fn.value.verify(false);
        ////if main_fn.datatype == DataType::Void && !verified {
        //self.builder
        //    .position_at_end(main_fn.value.get_last_basic_block().unwrap());
        //self.builder
        //    .build_return(Some(&self.context.i32_type().const_int(0, false)))
        //    .unwrap();
        //}

        return Ok(&self.module);
    }
}

impl<'ctx> CodeGen<'ctx> {
    fn compile_statement(
        &mut self,
        statement: &Statement,
        prefix: Option<String>,
    ) -> Result<Option<IntValue<'ctx>>, String> {
        match statement {
            Statement::FunctionDefinition(fndef) => {
                let fn_name = fndef.fn_name.clone();

                // Check if function is declared before
                let function = self
                    .functions
                    .get(&fn_name.lexeme)
                    .cloned()
                    .ok_or_else(|| format!("Function {} is not declared", fn_name.lexeme))?;

                // Create entry block
                let entry = self.context.append_basic_block(function.value, "entry");
                self.builder.position_at_end(entry);

                // save current function - to dynamically allocate variables to current function
                // scope

                self.current_fn = Some(function.clone());
                self.variables.clear();

                // Allocate parameters

                for (i, (name, datatype)) in fndef.fn_arguments.iter().enumerate() {
                    let param_value = function
                        .value
                        .get_nth_param(i as u32)
                        .unwrap()
                        .into_int_value();
                    let datatype = datatype.clone().unwrap();
                    let alloca = self
                        .create_entry_block_alloca(&function.value, name, &datatype)
                        .unwrap();

                    self.builder.build_store(alloca.0, param_value).unwrap();
                    self.variables.insert(
                        name.clone(),
                        Variable {
                            datatype: datatype,
                            ptr: alloca.0,
                            llvm_type: alloca.1,
                        },
                    );
                }

                // compile function body
                let mut last_value = None;
                for body_stmt in fndef.body.iter() {
                    last_value = self.compile_statement(body_stmt, None)?;
                }

                if self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    if let Some(val) = last_value {
                        self.builder.build_return(Some(&val)).unwrap();
                    } else {
                        let zero = self.context.i64_type().const_int(0, false);
                        self.builder.build_return(Some(&zero)).unwrap();
                    }
                }
                return Ok(None);
            }

            Statement::VariableDeclaration(vardecl) => {
                let expr = vardecl.clone().value.unwrap();
                let val = self.compile_expression(expr, None)?;

                if let Some(variable) = self.variables.get(&vardecl.name) {
                    self.builder.build_store(variable.ptr, val).unwrap();
                } else {
                    let current_fn = self.current_fn.clone().unwrap();
                    let datatype = vardecl.data_type.clone().unwrap();
                    let alloca = self.create_entry_block_alloca(
                        &current_fn.value,
                        &vardecl.name,
                        &datatype,
                    )?;
                    self.builder.build_store(alloca.0, val).unwrap();
                    self.variables.insert(
                        vardecl.name.clone(),
                        Variable {
                            datatype: datatype,
                            llvm_type: alloca.1,
                            ptr: alloca.0,
                        },
                    );
                }

                return Ok(Some(val));
            }

            Statement::Return(ret_stmt) => {
                if let Some(expr) = &ret_stmt.value {
                    let value = self.compile_expression(expr.clone(), None)?;
                    self.builder.build_return(Some(&value)).unwrap();
                    return Ok(Some(value));
                } else {
                    let function = self.current_fn.clone().unwrap();
                    if function.datatype != DataType::Void {
                        return Err(format!(
                            "Return value expected. Expected: {}",
                            function.datatype.to_str()
                        ));
                    }
                    self.builder.build_return(None).unwrap();

                    return Ok(None);
                }
            }

            Statement::Expr(expr) => {
                let value = self.compile_expression(expr.clone(), None)?;
                return Ok(Some(value))
            }
        }
    }

    fn compile_expression(
        &mut self,
        expression: Expr,
        expected: Option<DataType>,
    ) -> Result<IntValue<'ctx>, String> {
        // Todo : Check expected
        match expression {
            Expr::Literal(lit) => match lit {
                Literal::Number(n) => {
                    return Ok(self.context.i32_type().const_int(n as u64, false));
                }
                Literal::Boolean(b) => Ok(self.context.bool_type().const_int(b as u64, false)),
                _ => unimplemented!(),
            },

            Expr::Variable(var) => {
                let name = var.lexeme;
                let variable = self
                    .variables
                    .get(&name)
                    .ok_or_else(|| format!("Undefined variable {}", name))?;
                let val = self
                    .builder
                    .build_load(variable.llvm_type, variable.ptr, &name)
                    .unwrap();

                // TODO:
                // Return the corresponding type
                Ok(val.into_int_value())
            }

            Expr::Unary { op, right } => {
                let val = self.compile_expression(*right, None)?;

                match op {
                    ast::UnaryOp::Bang => return Ok(self.builder.build_not(val, "not").unwrap()),
                    ast::UnaryOp::Minus => {
                        return Ok(self.builder.build_int_neg(val, "neg").unwrap());
                    }
                }
            }

            Expr::Binary { left, op, right } => {
                let l = self.compile_expression(*left, None)?;
                let r = self.compile_expression(*right, None)?;

                use ast::BinaryOp;

                match op {
                    // Sweet killer!
                    // Sweet!
                    BinaryOp::Plus => return Ok(self.builder.build_int_add(l, r, "add").unwrap()),
                    BinaryOp::Minus => return Ok(self.builder.build_int_sub(l, r, "sub").unwrap()),
                    BinaryOp::Star => return Ok(self.builder.build_int_mul(l, r, "mul").unwrap()),
                    BinaryOp::Slash => return Ok(self.builder.build_int_signed_div(l, r, "div").unwrap()),
                    BinaryOp::Lesser => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLT, l, r, "lt").unwrap();
                        Ok(self
                            .builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "ext")
                            .unwrap())
                    },
                    BinaryOp::Greater => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SGT, l, r, "gt").unwrap();

                        Ok(self
                            .builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "ext")
                            .unwrap())
                    }
                    BinaryOp::LesserEqual => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLE, l, r, "le").unwrap();

                        Ok(self
                            .builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "ext")
                            .unwrap())
                    }
                    BinaryOp::GreaterEqual => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SGE, l, r, "ge").unwrap();
                        Ok(self
                            .builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "ext")
                            .unwrap())
                    }
                    BinaryOp::EqualEqual => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::EQ, l, r, "eq").unwrap();

                        Ok(self
                            .builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "ext")
                            .unwrap())
                    }
                    BinaryOp::NotEqual => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::NE, l, r, "ne").unwrap();
                        Ok(self
                            .builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "ext")
                            .unwrap())
                    }
                }

            },

            Expr::Grouping(expr) => {
                let value = self.compile_expression(*expr, None)?;
                return Ok(value);
            }

            Expr::FunctionCall{callee, args} => {
                //let callee = self.compile_expression(*callee, None);
                todo!();
            }

        }
    }

    fn compile_value(
        &mut self,
        value: Literal,
        _expected: Option<DataType>,
    ) -> (DataType, BasicValueEnum<'ctx>) {
        todo!();
    }

    fn create_main_function(&mut self) {}
}

impl<'ctx> CodeGen<'ctx> {
    fn declare_function(
        &mut self,
        name: &str,
        params: &[(String, DataType)],
        return_type: &DataType,
    ) -> Result<FunctionValue<'ctx>, String> {
        let ret_type = self.llvm_type(return_type)?;
        let param_types: Vec<BasicMetadataTypeEnum> = params
            .iter()
            .map(|(_, t)| self.llvm_type(t).unwrap().into())
            .collect();

        let mut arguments = Vec::new();
        for argument in params {
            arguments.push(argument.1.clone());
        }

        let fn_type = ret_type.fn_type(&param_types, false);
        let function = self.module.add_function(name, fn_type, None);

        // Set the parameter name for the function arguments
        for (i, (param_name, _)) in params.iter().enumerate() {
            function
                .get_nth_param(i as u32)
                .unwrap()
                .set_name(param_name);
        }

        self.functions.insert(
            name.to_string(),
            Function {
                name: name.to_string(),
                datatype: return_type.clone(),
                arguments: arguments,
                value: function,
            },
        );
        Ok(function)
    }

    fn create_entry_block_alloca(
        &self,
        function: &FunctionValue<'ctx>,
        name: &str,
        ty: &DataType,
    ) -> Result<(PointerValue<'ctx>, BasicTypeEnum<'ctx>), String> {
        let builder = self.context.create_builder();
        let entry = function.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(instr) => builder.position_before(&instr),
            None => builder.position_at_end(entry),
        }

        let llvm_type = self.llvm_type(ty).unwrap();
        Ok((
            builder.build_alloca(llvm_type, name).unwrap(),
            BasicTypeEnum::IntType(llvm_type),
        ))
    }
}

impl<'ctx> CodeGen<'ctx> {
    #[inline]
    fn get_basic_type(&self, datatype: DataType) -> BasicTypeEnum<'ctx> {
        todo!();
    }

    fn get_fn_type(
        &self,
        datatype: DataType,
        arguments: &[BasicMetadataTypeEnum<'ctx>],
        is_var_args: bool,
    ) -> FunctionType<'ctx> {
        todo!();
    }

    fn llvm_type(&self, typ: &DataType) -> Result<IntType<'ctx>, String> {
        match typ {
            DataType::I32 => return Ok(self.context.i32_type()),
            DataType::I64 => Ok(self.context.i64_type()),
            DataType::U32 => Ok(self.context.i32_type()),
            DataType::U64 => Ok(self.context.i32_type()),
            _ => unimplemented!(),
        }
    }
}
