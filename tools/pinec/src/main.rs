use clap::Parser as ClapParser;
use std::path::PathBuf;

use clap::{Args, Subcommand};

use lexer::lexer::{Token, lex};
use parser::parser::Parser;
use analyzer::analyzer::Analyzer;
use codegen::codegen::CodeGen;
use linker::linker;

use utils::read_from_file;

/// Alphera Compiler
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
    /// Build the AST and write it as a DOT graph
    // Feature needs to be build
    //Ast {
    //    /// Output DOT file
    //    //output: PathBuf,
    //},
    Ast,
    Tokens,
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
                Some(BuildMode::Ast) => {
                    // alpherac build ast -s main.alp output.dot
                    //print_ast_to_dot(build.source_file, output);
                    //alpherac build ast
                    build_ast(build.source_file)
                }
                Some(BuildMode::Tokens) => {
                    print_tokens(build.source_file);
                }
                Some(BuildMode::LlvmIr) => {
                    build_llvm_ir(build.source_file)
                }
            }
        }
    }
}

fn _print_ast_to_dot(source: PathBuf, out_file: PathBuf) {
    let source = handle_reading_file(&source);
    let tokens = lex(source.as_str());
    let mut parser = Parser::new(tokens);
    let res = parser.parse();
    match res {
        Ok(ast) => {
            parser.dump_ast(ast, out_file);
        }
        Err(err) => {
            eprintln!("{}", err);
        }
    }
}

fn build_ast(source: PathBuf) {
    let source = handle_reading_file(&source);
    let tokens = lex(source.as_str());
    let mut parser = Parser::new(tokens);
    let res = parser.parse();
    match res {
        Ok(ast) => {
            println!("{:#?}", ast)
        }
        Err(err) => {
            eprintln!("{}", err);
        }
    }
}

fn build_llvm_ir(file: PathBuf) {
    let source = handle_reading_file(&file);
    let tokens = lex(source.as_str());
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    //
    let file_name = file.to_str().unwrap();

    let module_name = file_name
        .split(".")
        .next()
        .map(|n| n.to_string())
        .unwrap_or(file_name.replace(".alp", ""));

    let ctx = CodeGen::create_context();
    let mut codegen = CodeGen::new(&ctx, &module_name);
    //println!("{:#?}", &ast);

    let module_ref = codegen.compile(&ast.unwrap()).unwrap();

    module_ref.print_to_stderr();
    if module_ref.verify().is_err() {
        panic!("Invalid LLVM IR");
    }
}

fn print_tokens(file: PathBuf) {
    let source = handle_reading_file(&file);
    let tokens = lex(source.as_str());
    _print_tokens(tokens.clone());
}

fn compile_program(file: PathBuf) {
    let source = handle_reading_file(&file);
    let tokens = lex(source.as_str());
    let mut parser = Parser::new(tokens);
    let mut ast = parser.parse().unwrap_or_else(|err| panic!("Couldn't parse the program due to: \n{}", err));

    let file_name = file.to_str().unwrap();

    let analyzer = Analyzer::new();
    if let Err(err) = analyzer.analyze(&mut ast) {
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

    let module_ref = codegen.compile(&ast).unwrap();

    if module_ref.verify().is_err() {
        module_ref.print_to_stderr();
        panic!("Invalid LLVM IR");
    }

    linker::ObjectCompiler::compile_module(&module_ref, &module_name);
    linker::ObjectLinker::link(&module_name, &module_name).unwrap();
}

fn handle_reading_file(source: &PathBuf) -> String {
    match read_from_file(source) {
        Ok(contents) => return contents,
        Err(e) => {
            panic!("ERROR: Could not open file due to: {}", e);
        }
    }
}

fn _print_tokens(tokens: Vec<Token>) {
    for token in tokens {
        println!("{}: {:?}", token.lexeme, token.token_type);
    }
}
