use ast::Statement;
use ast::types::DataType;
use ast::{BinaryOp, Expr, Literal, TypeEnv, UnaryOp, ModuleEnum, ImportType};

pub const INBUILT_FUNCTIONS: [&str; 2] = ["print", "println"];

pub struct Analyzer {
    pub loop_nest_level: i32,
    pub imports: ImportType,
    pub exports: TypeEnv,
}

impl Analyzer {
    pub fn new() -> Self {
        return Self {
            loop_nest_level: 0,
            imports: ImportType::new(),
            exports: TypeEnv::new(),
        };
    }

    pub fn reset(&mut self) {
        self.loop_nest_level = 0;
        self.imports = ImportType::new();
        self.exports = TypeEnv::new();
    }

    pub fn start_analysis(
        &mut self,
        compilation_unit: &mut ast::CompilationUnit,
    ) -> Result<(), String> {
        // populate exports for each module
        for (_, module) in compilation_unit.modules.iter_mut() {
            if let ModuleEnum::Module(modu) = module {
                self.analyze(&mut modu.ast, ImportType::new(), true)?;
                let exports = self.get_exports();
                modu.add_exports(exports);
                self.reset();
            }
        }

        // populate imports for each module
        let compilation_unit_clone = compilation_unit.clone();
        for (_, module) in compilation_unit.modules.iter_mut() {
            if let ModuleEnum::Module(modu) = module {
                for stmt in modu.ast.clone().iter() {
                    if let ast::Statement::Import(import_stmt) = stmt {
                        let module_name = import_stmt.import_name.clone();
                        if let Some(ModuleEnum::Module(import_module)) = compilation_unit_clone.get_module(&module_name) {
                            let imports = import_module.exports.clone();
                            modu.add_imports(import_stmt.import_name.clone(), imports);
                        } else {
                            return Err(format!("Could not find module: {}", module_name));
                        }
                    }
                }
            }
        }

        // analyze every modules
        for (_, module) in compilation_unit.modules.iter_mut() {
            if let ModuleEnum::Module(modu) = module {
                self.analyze(&mut modu.ast, modu.imports.clone(), false)?;
                self.reset();
            }
        }

        Ok(())
    }
}

impl Analyzer {
    pub fn analyze(
        &mut self,
        ast: &mut Vec<Statement>,
        imports: ImportType,
        only_exports: bool,
    ) -> Result<(), String> {
        let mut env = TypeEnv::new();
        self.imports = imports;
        // part of exports
        // Pass 1: collect function signatures
        for statement in ast.iter() {
            if let Statement::FunctionDefinition(fndef) = statement {
                let fndef = fndef.clone();
                let name = fndef.fn_name.lexeme;
                let return_type = fndef.ret_type;

                //let param_types: Vec<DataType> = params.iter().map(|x| x.1.clone()).collect();
                let func_type = DataType::Function {
                    params: fndef.fn_arguments,
                    ret_type: Box::new(return_type.clone()),
                };

                env.insert(name, func_type);
            }
        }

        if only_exports {
            self.exports = env.clone();
            return Ok(());
        }

        // Pass 2: collect alien function signatures
        for statement in ast.iter() {
            if let Statement::AlienDefinition(fndef) = statement {
                let fndef = fndef.clone();
                let name = fndef.fn_name;
                let params = fndef.fn_arguments;
                let return_type = fndef.ret_type;

                let func_type = DataType::Function {
                    params,
                    ret_type: Box::new(return_type.clone()),
                };

                env.insert(name, func_type);
            }
        }

        // Pass 4 Type check each statement
        for stmt in ast.iter_mut() {
            self.typecheck_statement(stmt, &mut env)?;
        }

        self.exports = env.clone();
        Ok(())
    }

    fn typecheck_statement(
        &mut self,
        stmt: &mut Statement,
        env: &mut TypeEnv,
    ) -> Result<DataType, String> {
        match stmt {
            Statement::FunctionDefinition(fndef) => {
                // Create a local env with parameters
                let mut local_env = env.clone();
                for (name, data_type) in fndef.fn_arguments.iter() {
                    local_env.insert(name.clone(), data_type.clone());
                }

                // Type check body type
                let mut body_type = DataType::Void;
                for body_stmt in fndef.body.iter_mut() {
                    body_type = self.typecheck_statement(body_stmt, &mut local_env)?;
                }

                // Verify return type matches (if not Unknown)
                if fndef.ret_type != DataType::Unknown && body_type != DataType::Void {
                    let _ = fndef.ret_type.unify(&body_type)?;
                }

                // Look into this
                return Ok(DataType::Unit);
            }

            Statement::Return(ret_stmt) => {
                self.typecheck_expr(ret_stmt.value.as_mut().unwrap(), env)?;
                Ok(ret_stmt.value.as_mut().unwrap().ty.clone())
            }

            Statement::Break(_) => {
                if self.loop_nest_level > 0 {
                    return Ok(DataType::Unit);
                }
                Err(format!("Break statement is only allowed inside loop"))
            }

            Statement::Continue(_) => {
                if self.loop_nest_level > 0 {
                    return Ok(DataType::Unit);
                }
                Err(format!("Continue statement is only allowed inside loop"))
            }

            Statement::VariableDeclaration(vardecl) => {
                self.typecheck_expr(&mut vardecl.value, env)?;
                let var_type = if let Some(ann) = vardecl.data_type.clone() {
                    let _ = ann.unify(&vardecl.value.ty)?;
                    ann.clone()
                } else {
                    vardecl.value.ty.clone()
                };

                // Should we actually store the types in both vardecl.data_type and
                // vardecl.value.ty ?
                vardecl.data_type = Some(var_type.clone());

                env.insert(vardecl.name.clone(), var_type.clone());
                // Ah, Uh..No variable decl must return unit
                //Ok(var_type)
                Ok(DataType::Unit)
            }

            Statement::Assignment(assign) => {
                self.typecheck_expr(&mut assign.value, env)?;

                if let Some(ty) = env.get(&assign.name) {
                    let _ = ty.unify(&assign.value.ty)?;
                    // Ah, Uh..No variable decl must return unit
                    //return Ok(ty.clone());
                    return Ok(DataType::Unit);
                } else {
                    // TODO: Pass the line, col here
                    return Err(format!(
                        "Variable not declared: {} at {}:{}",
                        assign.name, 69, 69
                    ));
                }
            }

            Statement::AlienDefinition(aliendef) => {
                // ensure none of types are unknown
                for (_, typ) in aliendef.fn_arguments.iter() {
                    if typ == &DataType::Unknown {
                        return Err(format!(
                            "alien function param cannot be Unknown: {} is Unknown",
                            typ.to_str()
                        ));
                    }
                }
                return Ok(DataType::Unit);
            }

            Statement::Expr(ex) => {
                self.typecheck_expr(ex, env)?;
                Ok(ex.ty.clone())
            }

            Statement::Import(import_stmt) => {
                // TODO: Implement type check
                Ok(DataType::Unit)
            }
        }
    }

    fn typecheck_expr(&mut self, expr: &mut ast::TypedExpr, env: &TypeEnv) -> Result<(), String> {
        match &mut expr.expr {
            Expr::Literal(lit) => {
                match lit {
                    // TODO: Check number range
                    Literal::Number(_) => expr.ty = DataType::I32,
                    Literal::Float(_) => expr.ty = DataType::F32,
                    Literal::Boolean(_) => expr.ty = DataType::Boolean,
                    Literal::String(_) => expr.ty = DataType::Str,
                    // It should have corresponding None representation in DataType
                    Literal::None => expr.ty = DataType::Void,
                }
            }
            // Have a dedicated Variable type in Parser. This is leaking from phase 1
            Expr::Variable { name, tok } => {
                if let Some(ty) = env.get(name.as_str()) {
                    expr.ty = ty.clone();
                //} else if let Some(ty) = self.imports.get(name.as_str()) {
                //    expr.ty = ty.clone();
                } else {
                    return Err(format!(
                        "Undefined variable: {} at {}:{}",
                        name, tok.line, tok.column
                    ));
                }
            }

            Expr::Unary { op, right: inner } => {
                self.typecheck_expr(inner, env)?;

                match op {
                    UnaryOp::Minus => {
                        // Modify for other types
                        if inner.ty != DataType::I32 {
                            return Err(format!(
                                "Cannot negate non-integer type: {}",
                                inner.ty.to_str()
                            ));
                        }
                        expr.ty = DataType::I32;
                    }
                    UnaryOp::Bang => {
                        if inner.ty != DataType::Boolean {
                            return Err(format!(
                                "Cannot negate non-boolean type: {}",
                                inner.ty.to_str()
                            ));
                        }
                        expr.ty = DataType::Boolean
                    }
                }
            }

            Expr::Binary { left, op, right } => {
                self.typecheck_expr(left, env)?;
                self.typecheck_expr(right, env)?;

                match op {
                    BinaryOp::Plus
                    | BinaryOp::Minus
                    | BinaryOp::Star
                    | BinaryOp::Slash
                    | BinaryOp::Mod => {
                        if left.ty != right.ty
                            && (left.ty != DataType::I32
                                || right.ty != DataType::I64
                                || right.ty != DataType::U32
                                || right.ty != DataType::U64
                                || right.ty != DataType::F32
                                || right.ty != DataType::F64)
                        {
                            return Err(format!(
                                "Binary Operation requires int | float operands, got {} and {}",
                                left.ty.to_str(),
                                right.ty.to_str()
                            ));
                        }
                        expr.ty = left.ty.clone()
                    }
                    BinaryOp::Lesser
                    | BinaryOp::Greater
                    | BinaryOp::LesserEqual
                    | BinaryOp::GreaterEqual => {
                        if !((left.ty == DataType::I32 && right.ty == DataType::I32)
                            || (left.ty == DataType::I64 && right.ty == DataType::I64)
                            || (left.ty == DataType::F32 && right.ty == DataType::F32)
                            || (left.ty == DataType::F64 && right.ty == DataType::F64))
                        {
                            return Err(format!(
                                "Invalid comparison operands. got {} and {}",
                                left.ty.to_str(),
                                right.ty.to_str()
                            ));
                        }
                        expr.ty = DataType::Boolean;
                    }

                    BinaryOp::And | BinaryOp::Or => {
                        if left.ty != DataType::Boolean || right.ty != DataType::Boolean {
                            return Err(format!(
                                "Logical operators can only operate on `bool` type. got {} and {}",
                                left.ty.to_str(),
                                right.ty.to_str()
                            ));
                        }
                        expr.ty = DataType::Boolean
                    }

                    BinaryOp::EqualEqual | BinaryOp::NotEqual => {
                        let _ = left.ty.unify(&right.ty)?;
                        expr.ty = DataType::Boolean;
                    }
                }
            }

            Expr::FunctionCall { name, callee, args } => {
                self.typecheck_expr(callee, env)?;

                let func_type = {
                    // check if function exists in current file
                    if env.get(name).is_some() {
                        env.get(name).unwrap().clone()
                    // Otherwise check if it is imported
                    //} else if self.imports.get(name).is_some() {
                    //    self.imports.get(name).unwrap().clone()
                    } else {
                        return Err(format!("Undefined function: {}", name));
                    }
                };

                if let DataType::Function { params, ret_type } = func_type {
                    if args.len() != params.len() {
                        return Err(format!(
                            "Function {} expectes {} arguments, got {} arguments",
                            name,
                            params.len(),
                            args.len()
                        ));
                    }

                    for (arg, param) in args.iter_mut().zip(params) {
                        self.typecheck_expr(arg, env)?;
                        let _ = param.1.unify(&arg.ty)?;
                    }

                    // Set the type
                    expr.ty = *ret_type;
                } else {
                    return Err(format!("{} is not a function", name));
                }
            }

            Expr::MethodCall { call_namespace, callee, args} => {
                // This is not pure method call structure
                // This assumes import calls as method calls
                assert_eq!(call_namespace.iter().len(), 2);

                let module_name = call_namespace.first().unwrap().clone();
                let name = call_namespace.iter().nth(1).unwrap().clone();
                if self.imports.get(&module_name).is_none() {
                    return Err(format!("Could not find {} in this scope.", module_name));
                }

                let module_namespace = self.imports.get(&module_name).unwrap(); 
                let func_type = if module_namespace.get(&name).is_none() {
                    return Err(format!("Could not find {} in this {}'s scope.", name, module_name));
                } else {
                    module_namespace.get(&name).unwrap().clone()
                };


                if let DataType::Function { params, ret_type } = func_type {
                    if args.len() != params.len() {
                        return Err(format!(
                            "Function {} expectes {} arguments, got {} arguments",
                            name,
                            params.len(),
                            args.len()
                        ));
                    }

                    for (arg, param) in args.iter_mut().zip(params) {
                        self.typecheck_expr(arg, env)?;
                        let _ = param.1.unify(&arg.ty)?;
                    }

                    // Set the type
                    expr.ty = *ret_type;
                } else {
                    return Err(format!("{} is not a function", name));
                }
            }

            Expr::Grouping(ex) => {
                self.typecheck_expr(ex, env)?;
                expr.ty = ex.ty.clone()
            }

            Expr::If {
                condition,
                if_block,
                else_block,
            } => {
                self.typecheck_expr(condition, env)?;

                if condition.ty != DataType::Boolean {
                    // Worst error message
                    // Improve this
                    return Err(format!(
                        "Only Expressions resolving to a boolean is allowed in if condition"
                    ));
                }

                let mut if_block_env = env.clone();
                let mut else_block_env = env.clone();

                let mut if_expr_ty = DataType::Unknown;
                let mut else_expr_ty = DataType::Unknown;

                for stmt in if_block.iter_mut() {
                    if_expr_ty = self.typecheck_statement(stmt, &mut if_block_env)?;
                }

                if else_block.is_some() {
                    for stmt in else_block.as_mut().unwrap().iter_mut() {
                        else_expr_ty = self.typecheck_statement(stmt, &mut else_block_env)?;
                    }
                } else {
                    else_expr_ty = if_expr_ty.clone()
                }

                if if_expr_ty != else_expr_ty {
                    return Err(format!(
                        "If and else block return mismatched types.\nIf block -> {}\nElse block -> {}",
                        if_expr_ty.to_str(),
                        else_expr_ty.to_str()
                    ));
                }

                expr.ty = if_expr_ty.clone();
            }

            Expr::While { condition, body } => {
                // increment the loop nest
                self.loop_nest_level += 1;
                // Typecheck the condition
                self.typecheck_expr(condition, env)?;

                // Verify that it resolves to boolean
                if condition.ty != DataType::Boolean {
                    return Err(format!(
                        "Only Expressions resolving to a boolean is allowed in while condition"
                    ));
                }

                let mut local_env = env.clone();

                for stmt in body.iter_mut() {
                    self.typecheck_statement(stmt, &mut local_env)?;
                }

                self.loop_nest_level -= 1;

                expr.ty = DataType::Unit;
            }
        }
        Ok(())
    }
}

impl Analyzer {
    fn get_imports(&self) -> ImportType {
        self.imports.clone()
    }

    fn get_exports(&self) -> TypeEnv {
        self.exports.clone()
    }
}

fn is_built_in(name: String) -> bool {
    if DataType::is_inbuilt_type(&name) {
        return true;
    }
    if INBUILT_FUNCTIONS.contains(&name.as_str()) {
        return true;
    }

    return false;
}

impl Analyzer {
    pub fn get_primitive_type(type_str: &str) -> DataType {
        match type_str {
            "i32" => DataType::I32,
            "i64" => DataType::I64,
            "u32" => DataType::U32,
            "u64" => DataType::U64,
            _ => DataType::Unknown,
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
        [DataType::I32, DataType::I64, DataType::U32, DataType::U64].contains(typ)
    }
}
