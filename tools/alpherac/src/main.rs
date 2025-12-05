use clap::Subcommand;
use clap::Parser as ClapParser;
use std::path::PathBuf;

use utils::read_from_file;
use lexer::lexer::{lex, Token};
use parser::parser::Parser;


/// Alphera Compiler
#[derive(ClapParser, Debug)]
#[clap(version)]
struct Args {
    //#[clap(short = 'p', long, env)]
    //garden_path: Option<PathBuf>,

    #[command(subcommand)]
    cmd: Commands,
}
#[derive(Subcommand, Debug)]
enum Commands {
    /// compile the source program 
    ///
    /// This command will compile the provided source file.
    Build {
        /// The source file to compile 
        #[clap(short, long)]
        source_file: PathBuf,
    },
}


fn main() {
    let args = Args::parse();

    match args.cmd {
        Commands::Build{source_file} => {
            compile_program(source_file)
        }
    }
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
