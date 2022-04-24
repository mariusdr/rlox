mod lexer;
mod parser;

use std::fs;
use std::env;
use parser::Parser;

fn print_result(res: &Result<parser::ast::Expr, parser::ParserError>) {
    if let Ok(ast) = res {
        println!("{}", ast);
    } else {
        println!("{:?}", res)
    }
}

fn main() -> Result<(), std::io::Error> {
    let src = "(5 - (3 - 1)) + -1";
    let mut parser = Parser::new(src);
    let res = parser.expression();
    print_result(&res);
    
    let src = "(x - (y - 1)) + -1";
    let mut parser = Parser::new(src);
    let res = parser.expression();
    print_result(&res);
    
    let src = "(5 - (3 - 1) + -1";
    let mut parser = Parser::new(src);
    let res = parser.expression();
    print_result(&res);

    Ok(())
}

// fn main() -> Result<(), std::io::Error> {
//     let args: Vec<String> = env::args().collect();
//     let src = fs::read_to_string(&args[1])?;

//     println!("{}", src);

//     println!("\n=========================\n");

//     let mut tokenstream = lexer::TokenStream::new(&src);
//     let mut peeker = tokenstream.peekable();

//     let tok = peeker.peek().unwrap();
//     println!("{}", tok);

//     // while let Some(tok) = tokenstream.next() {
//     //     println!("{}", tok);
//     // }

//     Ok(())
// }
