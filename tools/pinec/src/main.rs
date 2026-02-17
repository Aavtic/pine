use clap::Parser as ClapParser;
use std::path::PathBuf;

use clap::{Args, Subcommand};

use lexer::lexer::{Token, lex};
use parser::parser::Parser;
use analyzer::analyzer::Analyzer;
use codegen::codegen::{CodeGen, CodeGenModules};
use linker::linker;

use utils::handle_reading_file;

/// Pine Compiler
#[derive(ClapParser, Debug)]
#[clap(version)]
struct CMDArgs {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Build the program / artifacts
    Build(BuildArgs),
}

#[derive(Args, Debug)]
struct BuildArgs {
    /// Source file
    #[clap(short = 's', long = "source-file")]
    source_file: PathBuf,

    /// Optional subcommand for build (e.g. `ast`)
    #[command(subcommand)]
    mode: Option<BuildMode>,
}

#[derive(Subcommand, Debug)]
enum BuildMode {
    /// Build the token stream
    Tokens,
    /// Build the AST and write it as a DOT graph
    Ast,
    /// Build the type checked AST
    Analyze,
    /// Build the object file
    Object,
    /// Build the llvm-ir code
    LlvmIr,
}

fn main() {
    let args = CMDArgs::parse();

    match args.cmd {
        Commands::Build(build) => {
            match build.mode {
                None => {
                    // alpherac build -s main.alp
                    compile_program(build.source_file);
                }
                Some(BuildMode::Tokens) => {
                    print_tokens(build.source_file);
                }
                Some(BuildMode::Ast) => {
                    // alpherac build ast -s main.alp output.dot
                    //print_ast_to_dot(build.source_file, output);
                    //alpherac build ast
                    build_ast(build.source_file)
                }
                Some(BuildMode::Analyze) => {
                    build_analyze(build.source_file)
                }
                Some(BuildMode::Object) => {
                    build_object(build.source_file)
                }
                Some(BuildMode::LlvmIr) => {
                    build_llvm_ir(build.source_file)
                }
            }
        }
    }
}

fn _print_ast_to_dot(source_path: PathBuf, out_file: PathBuf) {
    let source = handle_reading_file(&source_path);
    let tokens = lex(source.as_str());
    let parent = PathBuf::from(source_path.parent().expect("Unable to find parent for source file"));
    let mut parser = Parser::new(tokens, parent);

    parser.parse("main").unwrap_or_else(|err| panic!("Couldn't parse the program due to: \n{}", err));
    let mut ast = parser.get_compilation_unit().get_module("main").unwrap().ast.clone();

    unimplemented!();

    //match ast {
    //    Ok(ast) => {
    //        parser.dump_ast(ast, out_file);
    //    }
    //    Err(err) => {
    //        eprintln!("{}", err);
    //    }
    //}
}

fn build_ast(source_path: PathBuf) {
    let source = handle_reading_file(&source_path);
    let tokens = lex(source.as_str());
    let parent = PathBuf::from(source_path.parent().expect("Unable to find parent for source file"));
    let mut parser = Parser::new(tokens, parent);

    parser.parse("main").unwrap_or_else(|err| panic!("Couldn't parse the program due to: \n{}", err));
    let ast = parser.get_compilation_unit();

    println!("{:#?}", ast);
}

fn build_analyze(file: PathBuf) {
    let source = handle_reading_file(&file);
    let tokens = lex(source.as_str());
    let parent = PathBuf::from(file.parent().expect("Unable to find parent for source file"));
    let mut parser = Parser::new(tokens, parent);
    parser.parse("main").unwrap_or_else(|err| panic!("Couldn't parse the program due to: \n{}", err));

    //let mut ast = parser.get_compilation_unit().get_module("main").unwrap().ast.clone();
    let mut compilation_unit = parser.get_compilation_unit().clone();

    let file_name = file.to_str().unwrap();

    let mut analyzer = Analyzer::new();
    if let Err(err) = analyzer.start_analysis(&mut compilation_unit) {
        eprintln!("Type Check failed due to:\n{}", err);
        return
    }

    println!("{:#?}", compilation_unit);
}

fn build_llvm_ir(file: PathBuf) {
    let source = handle_reading_file(&file);
    let tokens = lex(source.as_str());
    let parent = PathBuf::from(file.parent().expect("Unable to find parent for source file"));
    let mut parser = Parser::new(tokens, parent);
    parser.parse("main").unwrap_or_else(|err| panic!("Couldn't parse the program due to: \n{}", err));

    //let mut ast = parser.get_compilation_unit().get_module("main").unwrap().ast.clone();
    let mut compilation_unit = parser.get_compilation_unit().clone();

    let file_name = file.to_str().unwrap();

    let mut analyzer = Analyzer::new();
    if let Err(err) = analyzer.start_analysis(&mut compilation_unit) {
        eprintln!("Type Check failed due to:\n{}", err);
        return
    }

    let module_name = file_name
        .split(".")
        .next()
        .map(|n| n.to_string())
        .unwrap_or(file_name.replace(".alp", ""));


    let ctx = CodeGen::create_context();
    let mut codegen = CodeGen::new(&ctx, &module_name);
    //println!("{:#?}", &ast);

    let ctx = CodeGen::create_context();
    let mut codegen_mod = CodeGenModules::new();
    codegen_mod.compile(&ctx, compilation_unit.clone(), false);

    //module_ref.print_to_stderr();
    //if module_ref.verify().is_err() {
    //    panic!("Invalid LLVM IR");
    //}
}

fn build_object(file: PathBuf) {
    let source = handle_reading_file(&file);
    let tokens = lex(source.as_str());
    let parent = PathBuf::from(file.parent().expect("Unable to find parent for source file"));
    let mut parser = Parser::new(tokens, parent);
    parser.parse("main").unwrap_or_else(|err| panic!("Couldn't parse the program due to: \n{}", err));

    //let mut ast = parser.get_compilation_unit().get_module("main").unwrap().ast.clone();
    let mut compilation_unit = parser.get_compilation_unit().clone();

    let file_name = file.to_str().unwrap();

    let mut analyzer = Analyzer::new();
    if let Err(err) = analyzer.start_analysis(&mut compilation_unit) {
        eprintln!("Type Check failed due to:\n{}", err);
        return
    }

    let module_name = file_name
        .split(".")
        .next()
        .map(|n| n.to_string())
        .unwrap_or(file_name.replace(".alp", ""));

    let ctx = CodeGen::create_context();
    let mut codegen = CodeGen::new(&ctx, &module_name);
    //println!("{:#?}", &ast);

    let ctx = CodeGen::create_context();
    let mut codegen_mod = CodeGenModules::new();
    codegen_mod.compile(&ctx, compilation_unit.clone(), false);

    // Don't verify object because they may not be complete
    //if module_ref.verify().is_err() {
    //    module_ref.print_to_stderr();
    //    panic!("Invalid LLVM IR");
    //}

    //linker::ObjectCompiler::compile_module(&module_ref, &module_name);
    //linker::ObjectLinker::link(&module_name, &module_name).unwrap();
}

fn print_tokens(file: PathBuf) {
    let source = handle_reading_file(&file);
    let tokens = lex(source.as_str());
    _print_tokens(tokens.clone());
}

fn compile_program(file: PathBuf) {
    let source = handle_reading_file(&file);
    let tokens = lex(source.as_str());
    let parent = PathBuf::from(file.parent().expect("Unable to find parent for source file"));
    let mut parser = Parser::new(tokens, parent);
    parser.parse("main").unwrap_or_else(|err| panic!("Couldn't parse the program due to: \n{}", err));

    //let mut ast = parser.get_compilation_unit().get_module("main").unwrap().ast.clone();
    let mut compilation_unit = parser.get_compilation_unit().clone();

    let file_name = file.to_str().unwrap();

    let mut analyzer = Analyzer::new();
    if let Err(err) = analyzer.start_analysis(&mut compilation_unit) {
        eprintln!("Type Check failed due to:\n{}", err);
        return
    }

    let module_name = file_name
        .split(".")
        .next()
        .map(|n| n.to_string())
        .unwrap_or(file_name.replace(".alp", ""));


    let ctx = CodeGen::create_context();
    let mut codegen_mod = CodeGenModules::new();
    let modules =  codegen_mod.compile(&ctx, compilation_unit.clone(), false)
        .unwrap_or_else(|err| {
            eprintln!("ERROR When compiling:");
            panic!("{:?}", err);
    });


    let mut object_files = Vec::new();
    for (module_name, module_ref) in modules {
        println!("Compiling {}", module_name);
        let obj_file = linker::ObjectCompiler::compile_module(&module_ref, &module_name, file.parent().unwrap());
        object_files.push(obj_file);
    }
    linker::ObjectLinker::link(&module_name, object_files, linker::ObjectLinker::compile_runtime(&module_name)).unwrap();
}

fn _print_tokens(tokens: Vec<Token>) {
    for token in tokens {
        println!("{}: {:?}", token.lexeme, token.token_type);
    }
}
