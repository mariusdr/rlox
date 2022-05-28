mod cursor;
mod scanner;

pub mod token;
use scanner::Scanner;
use token::{Token, TokenType};

/// TokenStream is an iterator over the tokens produced by the lexer 
/// on a given source code string. 
/// It returns None after a Token with type Eof was returned once.
#[derive(Clone)]
pub struct TokenStream<'a> {
    scanr: Scanner<'a>,
    eof_reached: bool,
}

impl<'a> TokenStream<'a> {
    pub fn new(src: &'a str) -> Self {
        TokenStream {scanr: Scanner::new(src), eof_reached: false}
    }

    pub fn at_eof(&self) -> bool {
        self.eof_reached
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // scanner keeps returning EOF, track that here and 
        // end the iterator when EOF was returned once
        if self.at_eof() {
            return None;
        }
        
        let tokopt = self.scanr.scan_token();
        if let Some(tok) = &tokopt {
            if tok.get_type() == TokenType::Eof {
                self.eof_reached = true;
            }
        }
        tokopt
    }
}

