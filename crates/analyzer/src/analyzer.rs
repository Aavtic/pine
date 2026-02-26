use ast::Statement;
use ast::types::DataType;
use ast::{BinaryOp, Expr, Imports, Literal, Namespace, TypeEnv, UnaryOp};
use errors::errors::{ErrorMsg, ErrorCreation, IntoError};

pub const INBUILT_FUNCTIONS: [&str; 2] = ["print", "println"];

enum NamespaceType {
    Root,
    Namespace(Vec<String>),
}

pub struct Analyzer {
    pub loop_nest_level: i32,
    pub imports: Imports,
    pub exports: TypeEnv,

    module_name: String,
    package_name: String,
}

impl Analyzer {
    pub fn new() -> Self {
        return Self {
            loop_nest_level: 0,
            imports: Imports::new(),
            exports: TypeEnv::new(),

            module_name: "Unkonwn".into(),
            package_name: "Unknown".into(),
        };
    }

    pub fn reset(&mut self) {
        self.loop_nest_level = 0;
        self.imports = Imports::new();
        self.exports = TypeEnv::new();
    }

    pub fn start_analysis(
        &mut self,
        compilation_unit: &mut ast::CompilationUnit,
    ) -> Result<(), ErrorMsg> {
        let mut namespace_vec: ast::NamespaceType = vec![compilation_unit.get_root_namespace_mut().get_name()];
        // populate exports for each module
        self.populate_exports(compilation_unit.get_root_namespace_mut())?;

        // populate imports for each module
        self.populate_imports(compilation_unit.get_root_namespace_mut())?;

        // analyze every modules
        // namespace_vec is used to track the Namespace but in a list
        // this is used for printing the namespace for warnings or errors
        // This method is a little dirty
        self.run_analysis(compilation_unit.get_root_namespace_mut(), &mut namespace_vec)?;

        Ok(())
    }

    fn run_analysis(&mut self, space: &mut Namespace, _namespace_vec: &mut ast::NamespaceType) -> Result<(), ErrorMsg> {
        self.package_name = ast::namespace_to_string(_namespace_vec);
        let modules = space.get_all_modules_mut();

        for module in modules.iter_mut() {
            self.module_name = module.name.clone();
            self.analyze(
                &mut module.ast,
                module.imports.clone(),
                AnalysisMode::Semantic,
            )?;
            self.reset();
        }

        for (_, space) in space.get_all_namespaces_mut() {
            _namespace_vec.push(space.get_name());
            self.run_analysis(space, _namespace_vec)?;
            _namespace_vec.pop();
        }

        Ok(())
    }

    fn populate_imports(&mut self, space: &mut Namespace) -> Result<(), ErrorMsg> {
        let links = self.collect_imports(space.clone(), &mut Vec::new());
        self.link_imports(links, space)?;

        Ok(())
    }

    fn link_imports(
        &mut self,
        links: Vec<ast::Link>,
        namespace: &mut Namespace,
    ) -> Result<(), ErrorMsg> {
        for link in links {
            let exports = {
                let space = namespace
                    .get_namespace(&link.require_mod)
                    .ok_or_else(|| format!("Could not find module: {:?}", ast::namespace_to_string(&link.require_mod)))
                    .map_err(|err| {
                        err
                            .create_error()
                            .add_line(69)
                            .add_column(69)
                            .add_file_name(link.source_mod.last().unwrap())
                            .add_package_name(&ast::namespace_to_string(&link.source_mod))
                            .clone()
                    })?;
                let mut exp = TypeEnv::new();
                for module in space.get_all_modules() {
                    exp.extend(module.exports);
                }
                exp
            };

            let module = namespace
                .get_module_by_path_mut(&link.source_mod)
                .ok_or_else(|| format!("Could not find module: {:?}", &link.source_mod)
                            .create_error()
                            .add_line(69)
                            .add_column(69)
                            .add_file_name(link.source_mod.last().unwrap())
                            .add_package_name(&ast::namespace_to_string(&link.source_mod))
                            .clone()
                )?;
            module.add_imports(link.require_mod.last().unwrap().into(), exports, link.require_mod);
        }

        Ok(())
    }

    fn collect_imports(&self, space: Namespace, path: &mut Vec<String>) -> Vec<ast::Link> {
        let mut links = Vec::new();

        let modules = space.get_all_modules();

        for module in modules.iter() {
            for stmt in &module.ast {
                if let ast::Statement::Import(imp) = stmt {
                    // add the module name to the end for
                    // identifying the module later
                    // when injecting dependencies to module
                    let mut module_id = path.clone();
                    module_id.push(module.name.clone());

                    links.push(ast::Link {
                        source_mod: module_id,
                        require_mod: imp.import_namespace.clone(),
                    });
                }
            }
        }

        for (name, space) in space.get_all_namespaces() {
            path.push(name);
            links.extend(self.collect_imports(space, path));
            path.pop();
        }

        links
    }
    
    fn populate_exports(&mut self, namespace: &mut Namespace) -> Result<(), ErrorMsg> {
        for module in namespace.get_all_modules_mut() {
            self.analyze_exports(module)?;
        }

        for (_, space) in namespace.get_all_namespaces_mut() {
            self.populate_exports(space)?;
        }
        Ok(())
    }

    fn analyze_exports(&mut self, modu: &mut ast::Module) -> Result<(), ErrorMsg> {
        self.analyze(&mut modu.ast, Imports::new(), AnalysisMode::Exports)?;
        let exports = self.get_exports();
        modu.add_exports(exports);
        self.reset();

        Ok(())
    }
}

#[derive(PartialEq)]
enum AnalysisMode {
    Semantic,
    Exports,
}

impl Analyzer {
    fn analyze(
        &mut self,
        ast: &mut Vec<Statement>,
        imports: Imports,
        mode: AnalysisMode,
    ) -> Result<(), ErrorMsg> {
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

        if mode == AnalysisMode::Exports {
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
    ) -> Result<DataType, ErrorMsg> {
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
                    let _ = fndef.ret_type.unify(&body_type)
                        .map_err(|err| {
                        err
                            .create_error()
                            .add_line(69)
                            .add_column(69)
                            .add_file_name(&self.module_name)
                            .add_package_name(&self.package_name)
                            .clone()
                    });
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
                Err(format!("Break statement is only allowed inside loop")
                            .create_error()
                            .add_line(69)
                            .add_column(69)
                            .add_file_name(&self.module_name)
                            .add_package_name(&self.package_name)
                            .clone()
                )
            }

            Statement::Continue(_) => {
                if self.loop_nest_level > 0 {
                    return Ok(DataType::Unit);
                }
                Err(
                    format!("Continue statement is only allowed inside loop")
                            .create_error()
                            .add_line(69)
                            .add_column(69)
                            .add_file_name(&self.module_name)
                            .add_package_name(&self.package_name)
                            .clone()
                )
            }

            Statement::VariableDeclaration(vardecl) => {
                self.typecheck_expr(&mut vardecl.value, env)?;
                let var_type = if let Some(ann) = vardecl.data_type.clone() {
                    let _ = ann.unify(&vardecl.value.ty).map_err(|err| {
                        err
                            .create_error()
                            .add_line(69)
                            .add_column(69)
                            .add_file_name(&self.module_name)
                            .add_package_name(&self.package_name)
                            .clone()
                    });
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
                    let _ = ty.unify(&assign.value.ty).map_err(|err| {
                        err
                            .create_error()
                            .add_line(69)
                            .add_column(69)
                            .add_file_name(&self.module_name)
                            .add_package_name(&self.package_name)
                            .clone()

                    });
                    // Ah, Uh..No variable decl must return unit
                    //return Ok(ty.clone());
                    return Ok(DataType::Unit);
                } else {
                    // TODO: Pass the line, col here
                    return Err(
                        format!(
                        "Variable not declared")
                            .create_error()
                            .add_line(69)
                            .add_column(69)
                            .add_file_name(&self.module_name)
                            .add_package_name(&self.package_name)
                            .clone()
                    );
                }
            }

            Statement::AlienDefinition(aliendef) => {
                // ensure none of types are unknown
                for (_, typ) in aliendef.fn_arguments.iter() {
                    if typ == &DataType::Unknown {
                        return Err(
                            format!(
                            "alien function param cannot be Unknown: {} is Unknown",typ.to_str())
                            .create_error()
                            .add_line(69)
                            .add_column(69)
                            .add_file_name(&self.module_name)
                            .add_package_name(&self.package_name)
                            .clone()
                        );
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

    fn typecheck_expr(&mut self, expr: &mut ast::TypedExpr, env: &TypeEnv) -> Result<(), ErrorMsg> {
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
                    return Err(
                        format!("Undefined variable `{}`", tok.lexeme)
                            .create_error()
                            .add_line(tok.line)
                            .add_column(tok.column)
                            .add_file_name(&self.module_name)
                            .add_package_name(&self.package_name)
                            .clone()
                    );
                }
            }

            Expr::Unary { op, right: inner } => {
                self.typecheck_expr(inner, env)?;

                match op {
                    UnaryOp::Minus => {
                        // Modify for other types
                        if inner.ty != DataType::I32 {
                            return Err(format!("Cannot negate non-integer type: {}",inner.ty.to_str())
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                            );
                        }
                        expr.ty = DataType::I32;
                    }
                    UnaryOp::Bang => {
                        if inner.ty != DataType::Boolean {
                            return Err(format!("Cannot negate non-boolean type: {}", inner.ty.to_str())
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                            );
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
                            return Err(format!( "Binary Operation requires int | float operands, got {} and {}", left.ty.to_str(), right.ty.to_str())
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                            );
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
                            return Err(format!( "Invalid comparison operands. got {} and {}", left.ty.to_str(), right.ty.to_str())
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&&self.module_name)
                                .add_package_name(&&self.package_name)
                                .clone()
                            );
                        }
                        expr.ty = DataType::Boolean;
                    }

                    BinaryOp::And | BinaryOp::Or => {
                        if left.ty != DataType::Boolean || right.ty != DataType::Boolean {
                            return Err(format!( "Logical operators can only operate on `bool` type. got {} and {}", left.ty.to_str(), right.ty.to_str())
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()

                            );
                        }
                        expr.ty = DataType::Boolean
                    }

                    BinaryOp::EqualEqual | BinaryOp::NotEqual => {
                        let _ = left.ty.unify(&right.ty).map_err(|err| {
                            err
                            .create_error()
                            .add_line(69)
                            .add_column(69)
                            .add_file_name(&self.module_name)
                            .add_package_name(&self.package_name)
                            .clone()

                        });
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
                        return Err(format!("Undefined function: {}", name)
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                        );
                    }
                };

                if let DataType::Function { params, ret_type } = func_type {
                    if args.len() != params.len() {
                        return Err(format!(
                            "Function {} expectes {} arguments, got {} arguments", name, params.len(), args.len())
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()

                        );
                    }

                    for (arg, param) in args.iter_mut().zip(params) {
                        self.typecheck_expr(arg, env)?;
                        let _ = param.1.unify(&arg.ty)
                            .map_err(|err| {
                                err
                                    .create_error()
                                    .add_line(69)
                                    .add_column(69)
                                    .add_file_name(&self.module_name)
                                    .add_package_name(&self.package_name)
                                    .clone()

                            });
                    }

                    // Set the type
                    expr.ty = *ret_type;
                } else {
                    return Err(
                        format!("{} is not a function", name)
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                    );
                }
            }

            Expr::MethodCall {
                call_namespace,
                callee,
                args,
            } => {
                // This is not pure method call structure
                // This assumes import calls as method calls
                assert_eq!(call_namespace.iter().len(), 2);

                let module_name = call_namespace.first().unwrap().clone();
                let name = call_namespace.iter().nth(1).unwrap().clone();
                if self.imports.get(&module_name).is_none() {
                    return Err(
                        format!("Could not find {} in this scope.", module_name)
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                        );
                }

                let module_namespace = self.imports.get(&module_name).unwrap();
                let func_type = if module_namespace.get_import(&name).is_none() {
                    return Err(format!(
                        "Could not find {} in {}'s scope.",
                        name, module_name)
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                    );
                } else {
                    module_namespace.get_import(&name).unwrap().clone()
                };

                if let DataType::Function { params, ret_type } = func_type {
                    if args.len() != params.len() {
                        return Err(format!(
                            "Function {} expectes {} arguments, got {} arguments",
                            name,
                            params.len(),
                            args.len())
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                        );
                    }

                    for (arg, param) in args.iter_mut().zip(params) {
                        self.typecheck_expr(arg, env)?;
                        let _ = param.1.unify(&arg.ty).map_err(|err| {
                            err
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone();
                        });
                    }

                    // Set the type
                    expr.ty = *ret_type;
                } else {
                    return Err(format!("{} is not a function", name)
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                    );
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
                        "Only Expressions resolving to a boolean is allowed in if condition")
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                    );
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
                        else_expr_ty.to_str())
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                    );
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
                        "Only Expressions resolving to a boolean is allowed in while condition")
                                .create_error()
                                .add_line(69)
                                .add_column(69)
                                .add_file_name(&self.module_name)
                                .add_package_name(&self.package_name)
                                .clone()
                    );
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
    fn get_imports(&self) -> Imports {
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
