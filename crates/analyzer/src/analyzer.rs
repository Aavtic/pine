use ast::Statement;
use ast::types::DataType;
use ast::{BinaryOp, Expr, Literal, UnaryOp};

use std::collections::HashMap;

pub type TypeEnv = HashMap<String, DataType>;

pub const INBUILT_FUNCTIONS: [&str;2] = ["print", "println"];

pub struct Analyzer {
    pub loop_nest_level: i32
}

impl Analyzer {
    pub fn new() -> Self {
        return Self {
            loop_nest_level: 0,
        };
    }
}

impl Analyzer {
    pub fn analyze(&mut self, ast: &mut Vec<Statement>) -> Result<(), String> {
        let mut env = TypeEnv::new();
        // Pass 1: collect function signatures
        for statement in ast.iter() {
            if let Statement::FunctionDefinition(fndef) = statement {
                let fndef = fndef.clone();
                let name = fndef.fn_name.lexeme;
                let params = fndef.fn_arguments;
                let return_type = fndef.ret_type;

                let param_types: Vec<DataType> = params.iter().map(|x| x.1.clone()).collect();
                let func_type = DataType::Function {
                    params: param_types,
                    ret_type: Box::new(return_type.clone()),
                };

                env.insert(name, func_type);
            }
        }

        // Pass 2: collect alien function signatures
        for statement in ast.iter() {
            if let Statement::AlienDefinition(fndef) = statement {
                let fndef = fndef.clone();
                let name = fndef.fn_name;
                let params = fndef.fn_arguments;
                let return_type = fndef.ret_type;

                let param_types: Vec<DataType> = params.iter().map(|x| x.1.clone()).collect();
                let func_type = DataType::Function {
                    params: param_types,
                    ret_type: Box::new(return_type.clone()),
                };

                env.insert(name, func_type);
            }
        }

        // Pass 3 add inbuilt function signatures

        for statement in ast.iter() {
            if let Statement::FunctionDefinition(fndef) = statement {
                let fndef = fndef.clone(); let name = fndef.fn_name.lexeme;
                let params = fndef.fn_arguments;
                let return_type = fndef.ret_type;

                let param_types: Vec<DataType> = params.iter().map(|x| x.1.clone()).collect();
                let func_type = DataType::Function {
                    params: param_types,
                    ret_type: Box::new(return_type.clone()),
                };

                env.insert(name, func_type);
            }
        }

        // Pass 4 Type check each statement

        for stmt in ast.iter_mut() {
            self.typecheck_statement(stmt, &mut env)?;
        }

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
                    return Ok(DataType::Unit)
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
                return Ok(DataType::Unit)
            }

            Statement::Expr(ex) => {
                self.typecheck_expr(ex, env)?;
                Ok(ex.ty.clone())
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
                    BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Star | BinaryOp::Slash | BinaryOp::Mod => {
                        if left.ty != right.ty && (
                            left.ty != DataType::I32
                            || right.ty != DataType::I64
                            || right.ty != DataType::U32
                            || right.ty != DataType::U64
                            || right.ty != DataType::F32
                            || right.ty != DataType::F64 )
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
                        if !((left.ty == DataType::I32 && right.ty == DataType::I32) || 
                            (left.ty == DataType::I64 && right.ty == DataType::I64) ||
                            (left.ty == DataType::F32 && right.ty == DataType::F32) ||
                            (left.ty == DataType::F64 && right.ty == DataType::F64)
                            ) {
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

                let func_type = env
                    .get(name)
                    .ok_or_else(|| format!("Undefined function: {}", name))?
                    .clone();

                if let DataType::Function { params, ret_type } = func_type {
                    if args.len() != params.len() {
                        return Err(format!(
                            "Function {} expectes {} arguments, got {} arguments",
                            name,
                            params.len(),
                            args.len()
                        ));
                    }

                    for (arg, param_type) in args.iter_mut().zip(params) {
                        self.typecheck_expr(arg, env)?;
                        let _ = param_type.unify(&arg.ty)?;
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
                    return Err(format!("Only Expressions resolving to a boolean is allowed in if condition"));
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
                } else { else_expr_ty = if_expr_ty.clone() }

                if if_expr_ty != else_expr_ty {
                    return Err(format!("If and else block return mismatched types.\nIf block -> {}\nElse block -> {}", if_expr_ty.to_str(), else_expr_ty.to_str()));
                }

                expr.ty = if_expr_ty.clone();
            }

            Expr::While {
                condition,
                body,
            } => {
                // increment the loop nest
                self.loop_nest_level += 1;
                // Typecheck the condition
                self.typecheck_expr(condition, env)?;

                // Verify that it resolves to boolean
                if condition.ty != DataType::Boolean {
                    return Err(format!("Only Expressions resolving to a boolean is allowed in while condition"));
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
