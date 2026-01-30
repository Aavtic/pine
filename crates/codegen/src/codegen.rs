use std::collections::HashMap;

use crate::{function::Function, variable::Variable};

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
                    input.push((arg.0.clone(), arg.1.clone()))
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
                    let datatype = datatype.clone();
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
                let expr = vardecl.clone().value;
                let val = self.compile_expression(expr.expr, Some(expr.ty))?;

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

            Statement::Assignment(assign) => {
                let compiled_value = self.compile_expression(assign.value.expr.clone(), Some(assign.value.ty.clone())).unwrap();
                if let Some(variable) = self.variables.get(&assign.name) {
                    self.builder.build_store(variable.ptr, compiled_value).unwrap();
                } else {
                    panic!("Variable not defined!, This is unreachable");
                }

                Ok(Some(compiled_value))
            }

            Statement::Return(ret_stmt) => {
                if let Some(expr) = &ret_stmt.value {
                    let value = self.compile_expression(expr.expr.clone(), Some(expr.ty.clone()))?;
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
                let value = self.compile_expression(expr.expr.clone(), Some(expr.ty.clone()))?;
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

            Expr::Variable{name, ..} => {
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
                let val = self.compile_expression(right.expr, Some(right.ty))?;

                match op {
                    ast::UnaryOp::Bang => return Ok(self.builder.build_not(val, "not").unwrap()),
                    ast::UnaryOp::Minus => {
                        return Ok(self.builder.build_int_neg(val, "neg").unwrap());
                    }
                }
            }

            Expr::Binary { left, op, right } => {
                let l = self.compile_expression(left.expr, Some(left.ty))?;
                let r = self.compile_expression(right.expr, Some(right.ty))?;

                use ast::BinaryOp;

                match op {
                    // Sweet killer!
                    // Sweet!
                    BinaryOp::Plus => return Ok(self.builder.build_int_add(l, r, "add").unwrap()),
                    BinaryOp::Minus => return Ok(self.builder.build_int_sub(l, r, "sub").unwrap()),
                    BinaryOp::Star => return Ok(self.builder.build_int_mul(l, r, "mul").unwrap()),
                    BinaryOp::Slash => return Ok(self.builder.build_int_signed_div(l, r, "div").unwrap()),
                    BinaryOp::Mod   => return Ok(self.builder.build_int_signed_rem(l, r, "mod").unwrap()),
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
                let value = self.compile_expression(expr.expr, Some(expr.ty))?;
                return Ok(value);
            }

            Expr::FunctionCall{callee, args, name} => {
                // TODO resolve callee
                //let callee = self.compile_expression(callee.expr, Some(callee.ty))?;

                let function = self.functions
                    .get(&name)
                    .cloned()
                    .ok_or_else(|| format!("Undefined function: {}", name))?;

                let arg_values: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|a| self.compile_expression(a.expr.clone(), Some(a.ty.clone())).map(|v| v.into()))
                    .collect::<Result<_, _>>()?;

                let call = self.
                    builder.
                    build_call(function.value, &arg_values, "call")
                    .unwrap();

                Ok(call.try_as_basic_value().unwrap_basic().into_int_value())
            }

            Expr::If{
                condition,
                if_block,
                else_block,
            } => {
                let condition_val = self.compile_expression(condition.expr.clone(), Some(condition.ty.clone()))?;
                // Build i1 for branching
                let cond_bool = self
                    .builder
                    .build_int_truncate(condition_val, self.context.bool_type(), "cond")
                    .unwrap();
                
                let function = self.current_fn.as_ref().unwrap();
                let then_bb = self.context.append_basic_block(function.value, "then");
                let else_bb = self.context.append_basic_block(function.value, "else");
                let merge_bb = self.context.append_basic_block(function.value, "merge");

                self.builder.build_conditional_branch(cond_bool, then_bb, else_bb).unwrap();

                // Then block
                self.builder.position_at_end(then_bb);
                let mut then_val = self.context.i64_type().const_int(0, false);

                for stmt in if_block {
                    if let Some(val) = self.compile_statement(&stmt, None)? {
                        then_val = val;
                    }
                }

                let then_end = self.builder.get_insert_block().unwrap();
                let then_has_terminator = then_end.get_terminator().is_some();

                if !then_has_terminator {
                    // Go to merge block because there is no terminator in if block
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                // Else block
                self.builder.position_at_end(else_bb);
                let mut else_val = then_val.get_type().const_int(0, false);

                if else_block.is_some() {
                    for stmt in else_block.unwrap() {
                        if let Some(val) = self.compile_statement(&stmt, None)? {
                            else_val = val;
                        }
                    }
                }

                let else_end = self.builder.get_insert_block().unwrap();
                let else_has_terminator = else_end.get_terminator().is_some();

                if !else_has_terminator {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                // Merge only if one of the branch reaches it
                // if none then delete the merge block

                if then_has_terminator && else_has_terminator {
                    // Both branches return/terminate, merge block is unreachable
                    // Remove it and return a dummy value
                    unsafe {
                        merge_bb.delete().unwrap();
                    }
                    // Return a dummy value - the actual return happened in the branches
                    return Ok(self.context.i64_type().const_int(0, false))
                } else {
                    self.builder.position_at_end(merge_bb);

                    let phi_ty = then_val.get_type();
                    assert_eq!(then_val.get_type(), else_val.get_type());

                    let phi = self
                        .builder
                        .build_phi(phi_ty, "phi")
                        .unwrap();

                    if !then_has_terminator {
                        phi.add_incoming(&[(&then_val, then_end)]);
                    }

                    if !else_has_terminator {
                        phi.add_incoming(&[(&else_val, else_end)]);
                    }

                    return Ok(phi.as_basic_value().into_int_value())
                }
            }

            Expr::While {
                condition,
                body
            } => {
                let function = self.current_fn.as_ref().unwrap();
                let cond_bb = self.context.append_basic_block(function.value, "while_cond");
                let body_bb = self.context.append_basic_block(function.value, "while_body");
                let end_bb = self.context.append_basic_block(function.value, "while_end");

                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(cond_bb);

                let cond_value = self.compile_expression(condition.expr.clone(), Some(condition.ty))?;
                let cond_bool = self
                    .builder
                    .build_int_truncate(cond_value, self.context.bool_type(), "cond")
                    .unwrap();

                self.builder
                    .build_conditional_branch(cond_bool, body_bb, end_bb)
                    .unwrap();

                self.builder
                    .position_at_end(body_bb);

                for stmt in body {
                    self.compile_statement(&stmt, None)?;
                }

                // Check if there are any terminators at the end of the while block
                // if not then unconditionally go to the condition to continue the loop
                if self.builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none() {
                        self.builder
                            .build_unconditional_branch(cond_bb).unwrap();
                }

                // End
                self.builder.position_at_end(end_bb);
                Ok(self.context.i64_type().const_int(0, false))
            }
        }
    }

    fn compile_value(
        &mut self,
        value: Literal,
        _expected: Option<DataType>,
    ) -> (DataType, BasicValueEnum<'ctx>) {

        while if 1 > 0 { true } else { false } {
            println!("hello there!");
        }

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
            DataType::Boolean => Ok(self.context.bool_type()),
            _ => unimplemented!("{}", format!("{} is not implemented", typ.to_str())),
        }
    }
}
