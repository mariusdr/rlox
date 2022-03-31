mod lexer;
use std::fs;
use std::env;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    let src = fs::read_to_string(&args[1])?;

    println!("{}", src);

    println!("\n=========================\n");

    let mut tokenstream = lexer::TokenStream::new(&src);
    while let Some(tok) = tokenstream.next() {
        println!("{}", tok);
    }

    Ok(())
}
