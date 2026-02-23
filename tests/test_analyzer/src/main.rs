use clap::Parser as ClapParser;
use std::path::PathBuf;

use clap::{Args, Subcommand};

use analyzer::analyzer::Analyzer;
use lexer::lexer::{Token, lex};
use parser::parser::Parser;
//use codegen::codegen::{CodeGen, CodeGenModules};
//use linker::linker;
//
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
                Some(BuildMode::Analyze) => build_analyze(build.source_file),
                Some(BuildMode::Object) => build_object(build.source_file),
                Some(BuildMode::LlvmIr) => build_llvm_ir(build.source_file),
            }
        }
    }
}

fn _print_ast_to_dot(source_path: PathBuf, out_file: PathBuf) {
    let source = handle_reading_file(&source_path);
    let tokens = lex(source.as_str());
    let parent = PathBuf::from(
        source_path
            .parent()
            .expect("Unable to find parent for source file"),
    );
    let mut parser = Parser::new(tokens, parent);

    parser
        .parse("main", vec!["root".into()], false)
        .unwrap_or_else(|err| panic!("Couldn't parse the program due to: \n{}", err));
    //let mut ast = parser.get_compilation_unit().get_module("main").unwrap().ast.clone();

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
    let parent = PathBuf::from(
        source_path
            .parent()
            .expect("Unable to find parent for source file"),
    );
    let mut parser = Parser::new(tokens, parent);

    parser
        .parse("main", vec!["main".into()], true)
        .unwrap_or_else(|err| panic!("Couldn't parse the program due to: \n{}", err));
    let ast = parser.get_compilation_unit();

    println!("{:#?}", ast);
}

fn build_analyze(file: PathBuf) {
    let source = handle_reading_file(&file);
    let tokens = lex(source.as_str());
    let parent = PathBuf::from(
        file.parent()
            .expect("Unable to find parent for source file"),
    );
    let mut parser = Parser::new(tokens, parent);
    let namespace = vec!["main".into()];
    parser
        .parse("main", namespace.clone(), true)
        .unwrap_or_else(|err| panic!("Couldn't parse the program due to: \n{}", err));

    //let mut ast = parser.get_compilation_unit().get_module("main").unwrap().ast.clone();
    let mut compilation_unit = parser.get_compilation_unit().clone();

    let mut analyzer = Analyzer::new();
    if let Err(err) = analyzer.start_analysis(&mut compilation_unit) {
        eprintln!("Type Check failed due to:\n{}", err);
        return;
    }

    println!("{:#?}", compilation_unit);
}

fn build_llvm_ir(file: PathBuf) {
    unimplemented!();
}

fn build_object(file: PathBuf) {
    unimplemented!();
}

fn print_tokens(file: PathBuf) {
    unimplemented!();
}

fn compile_program(file: PathBuf) {
    unimplemented!();
}

fn _print_tokens(tokens: Vec<Token>) {
    for token in tokens {
        println!("{}: {:?}", token.lexeme, token.token_type);
    }
}
