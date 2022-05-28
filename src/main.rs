mod lexer;
mod parser;

use std::fs;
use std::env;
use std::fmt;
use parser::Parser;

fn print_result<ParseRes: fmt::Display>(res: &Result<ParseRes, parser::ParserError>) {
    match res {
        Ok(ast) => println!("{}", ast),
        Err(err) => println!("{:?}", err),
    }
}

fn print_results<ParseRes: fmt::Display>(res: &Result<Vec<ParseRes>, parser::ParserError>) {
    if let Ok(inner) = res {
        for pr in inner {
            println!("{}", pr);
        }
    }
    if let Err(et) = res {
        println!("{:?}", et);
    }
}

fn main() -> Result<(), std::io::Error> {
    // let src = "print (1232+232*y);";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);
    
    // let src = "var x;";
    // let mut parser = Parser::new(src);
    // let res = parser.declaration();
    // print_result(&res);
    
    // let src = "var x= 232;";
    // let mut parser = Parser::new(src);
    // let res = parser.declaration();
    // print_result(&res);
    
    // let src = "var x= s*232*(1+2+3+4+u);";
    // let mut parser = Parser::new(src);
    // let res = parser.declaration();
    // print_result(&res);

    // let src = "x=42;";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);
    
    // let src = "x=y=z=42;";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);

    // let src = "42=12;";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);

    // let src = "(232+1323+x)=12;";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);


    // let src = "{ var x=1; var y=2; var z=x+y; }";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);
    
    // let src = "{ x=1; {var x=1;} {var y=2;} {{var z=x+y;} print z;} }";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);
    
    // let src = "if (x == 1) print zoid; else print berg;";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);
    
    let src = "if print zoid; else print berg;";
    let mut parser = Parser::new(src);
    let res = parser.statement();
    print_result(&res);
    
    let src = "if (x print zoid; else print berg;";
    let mut parser = Parser::new(src);
    let res = parser.statement();
    print_result(&res);
    
    let src = "if (x);";
    let mut parser = Parser::new(src);
    let res = parser.statement();
    print_result(&res);
    
    // let src = "if (x == 1) {var x = \"zoid\"; print x;} else print \"berg\";";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);
    
    // let src = "{ }";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);
    
    // let src = "{}";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);
    
    // let src = "{{}{{}}}";
    // let mut parser = Parser::new(src);
    // let res = parser.statement();
    // print_result(&res);

    // ----------------------------------------------------------------------//

    // let src = "(5 - (3 - 1)) + -1";
    // let mut parser = Parser::new(src);
    // let res = parser.expression();
    // print_result(&res);
    
    // let src = "(x - (y - 1)) + -1";
    // let mut parser = Parser::new(src);
    // let res = parser.expression();
    // print_result(&res);
    
    // let src = "(5 - (3 - 1) + -1";
    // let mut parser = Parser::new(src);
    // let res = parser.expression();
    // print_result(&res);
    
    // let src = "1 < 2";
    // let mut parser = Parser::new(src);
    // let res = parser.expression();
    // print_result(&res);
    
    // let src = "1 <= 2";
    // let mut parser = Parser::new(src);
    // let res = parser.expression();
    // print_result(&res);
    
    // let src = "1 >= 2";
    // let mut parser = Parser::new(src);
    // let res = parser.expression();
    // print_result(&res);
    
    // let src = "1 != 2";
    // let mut parser = Parser::new(src);
    // let res = parser.expression();
    // print_result(&res);
    
    // let src = "1 == 2";
    // let mut parser = Parser::new(src);
    // let res = parser.expression();
    // print_result(&res);
    
    // let src = "foo == bar";
    // let mut parser = Parser::new(src);
    // let res = parser.expression();
    // print_result(&res);

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
