use clap::Parser as ClapParser;
use std::path::PathBuf;

use clap::{Subcommand, Args};

use utils::read_from_file;
use lexer::lexer::{lex, Token};
use parser::parser::Parser;


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
    Ast {
        /// Output DOT file
        output: PathBuf,
    },
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
                Some(BuildMode::Ast { output }) => {
                    // alpherac build ast -s main.alp output.dot
                    print_ast_to_dot(build.source_file, output);
                }
            }
        }
    }
}

fn print_ast_to_dot(source: PathBuf, out_file: PathBuf) {
    let source = handle_reading_file(source);
    let tokens = lex(source.as_str());
    print_tokens(tokens.clone());
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    parser.dump_ast(ast, out_file);
}

fn compile_program(file: PathBuf) {
    let source = handle_reading_file(file);
    let tokens = lex(source.as_str());
    print_tokens(tokens.clone());
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    println!("{:?}", ast);
}

fn handle_reading_file(source: PathBuf) -> String {
    match read_from_file(source) {
        Ok(contents) => return contents,
        Err(e) => {
            panic!("ERROR: Could not open file due to: {}", e);
        }
    }
}

fn print_tokens(tokens: Vec<Token>) {
    for token in tokens {
        println!("{}: {:?}", token.lexeme, token.token_type);
    }
}
