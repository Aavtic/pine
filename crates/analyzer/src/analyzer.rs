use ast::Statement;
use ast::types::DataType;
use ast::{BinaryOp, Expr, Literal, UnaryOp};

use std::collections::HashMap;

pub type TypeEnv = HashMap<String, DataType>;

pub struct Analyzer {}

impl Analyzer {
    pub fn new() -> Self {
        return Self {};
    }
}

impl Analyzer {
    pub fn analyze(&self, ast: &mut Vec<Statement>) -> Result<(), String> {
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

        // Pass 2 Type check each statement

        for stmt in ast.iter_mut() {
            self.typecheck_statement(stmt, &mut env)?;
        }

        Ok(())
    }

    fn typecheck_statement(
        &self,
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
                return Ok(DataType::Void);
            }

            Statement::Return(ret_stmt) => {
                self.typecheck_expr(ret_stmt.value.as_mut().unwrap(), env)?;
                Ok(ret_stmt.value.as_mut().unwrap().ty.clone())
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
                Ok(var_type)
            }

            Statement::Assignment(assign) => {
                self.typecheck_expr(&mut assign.value, env)?;

                if let Some(ty) = env.get(&assign.name) {
                    let _ = ty.unify(&assign.value.ty)?;
                    return Ok(ty.clone());
                } else {
                    // TODO: Pass the line, col here
                    return Err(format!(
                        "Variable not declared: {} at {}:{}",
                        assign.name, 69, 69
                    ));
                }
            }

            Statement::Expr(ex) => {
                self.typecheck_expr(ex, env)?;
                Ok(ex.ty.clone())
            }
        }
    }

    fn typecheck_expr(&self, expr: &mut ast::TypedExpr, env: &TypeEnv) -> Result<(), String> {
        match &mut expr.expr {
            Expr::Literal(lit) => {
                match lit {
                    // TODO: Check number range
                    Literal::Number(_) => expr.ty = DataType::I32,
                    Literal::Boolean(_) => expr.ty = DataType::Boolean,
                    Literal::String(_) => expr.ty = DataType::String,
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
                        inner.ty = DataType::Boolean;
                    }
                }
            }

            Expr::Binary { left, op, right } => {
                self.typecheck_expr(left, env)?;
                self.typecheck_expr(right, env)?;

                match op {
                    BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Star | BinaryOp::Slash => {
                        if left.ty != DataType::I32 || right.ty != DataType::I32 {
                            return Err(format!(
                                "Binary Operation requires int operands, got {} and {}",
                                left.ty.to_str(),
                                right.ty.to_str()
                            ));
                        }
                        expr.ty = DataType::I32
                    }
                    BinaryOp::Lesser
                    | BinaryOp::Greater
                    | BinaryOp::LesserEqual
                    | BinaryOp::GreaterEqual => {
                        if left.ty != DataType::I32 || right.ty != DataType::I32 {
                            return Err(format!(
                                "Comparision requires int operands got {} and {}",
                                left.ty.to_str(),
                                right.ty.to_str()
                            ));
                        }
                        expr.ty = DataType::I32;
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
                        let _ = arg.ty.unify(&param_type)?;
                    }

                    expr.ty = *ret_type;
                } else {
                    return Err(format!("{} is not a function", name));
                }
            }

            Expr::Grouping(ex) => {
                self.typecheck_expr(ex, env)?;
                expr.ty = ex.ty.clone()
            }
        }

        Ok(())
    }
}

impl Analyzer {
    pub fn get_primitive_type(type_str: &str) -> DataType {
        match type_str {
            "i32" => DataType::I32,
            "i64" => DataType::I64,
            "u32" => DataType::U32,
            "u64" => DataType::U64,
            "string" => DataType::String,
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
