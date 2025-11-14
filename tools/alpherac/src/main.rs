use lexer::lexer;

fn main() {
    let program = r#"
    include std;

    fn main() {
        print(1)
    }
    "#;
    let tokens = lexer::lexer::lex(program);

    for token in tokens {
        println!("{}: {:?}", token.lexeme, token.token_type);
    }
}
