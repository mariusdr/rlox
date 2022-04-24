pub mod ast;
use std::result::Result;
use std::iter::Peekable;
use std::fmt;
use std::f64;

use ast::*;
use super::lexer::TokenStream;
use super::lexer::token::{Token, TokenType};

pub struct Parser<'a> {
    toks: Peekable<TokenStream<'a>>,
    previous: Token,
}

#[derive(Debug)]
pub struct ParserError {
    source: String,
}

impl ParserError {
    pub fn new(source: String) -> Self {
        ParserError { source: source }
    }

    pub fn from(source: &str) -> Self {
        ParserError { source: String::from(source) }
    }
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Parser { 
            toks: TokenStream::new(src).peekable(), 
            previous: Token::new(TokenType::Eof, None, 0) 
        }
    }

    pub fn expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }

    /// equality ::= comparison ('!=' | '==') comparison | comparison
    pub fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.comparison()?;

        while self.try_consume(TokenType::BangEqual) || self.try_consume(TokenType::EqualEqual) {
            let op = BinaryOpType::extract(&self.previous).unwrap();
            let rhs = self.comparison()?;
            lhs = Expr::BinaryOp(BinaryOpData::new(op, Box::new(lhs), Box::new(rhs)));
        }
        Ok(lhs)
    }

    /// comparison ::= term ('<' | '<=' | '>' | '>=') term
    pub fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.term()?;

        while self.try_consume(TokenType::Greater) || self.try_consume(TokenType::GreaterEqual) ||
              self.try_consume(TokenType::Less) || self.try_consume(TokenType::LessEqual) 
        {
            let op = BinaryOpType::extract(&self.previous).unwrap();
            let rhs = self.term()?;
            lhs = Expr::BinaryOp(BinaryOpData::new(op, Box::new(lhs), Box::new(rhs)));
        }
        Ok(lhs)
    }

    /// term ::= factor ('+' | '-') factor | factor
    pub fn term(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.factor()?;

        while self.try_consume(TokenType::Minus) || self.try_consume(TokenType::Plus) {
            let op = BinaryOpType::extract(&self.previous).unwrap();
            let rhs = self.factor()?;
            lhs = Expr::BinaryOp(BinaryOpData::new(op, Box::new(lhs), Box::new(rhs)));
        }
        Ok(lhs)
    }

    /// factor ::= unary ('/' | '*') factor | unary
    pub fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.unary()?;
        
        while self.try_consume(TokenType::Slash) || self.try_consume(TokenType::Star) {
            let op = BinaryOpType::extract(&self.previous).unwrap();
            let rhs = self.factor()?;
            lhs = Expr::BinaryOp(BinaryOpData::new(op, Box::new(lhs), Box::new(rhs)));
        }
        Ok(lhs)
    }

    /// unary ::= '-' unary | '!' unary
    ///       ::= primary
    pub fn unary(&mut self) -> Result<Expr, ParserError> {
        if self.try_consume(TokenType::Bang) | self.try_consume(TokenType::Minus) {
            let op = UnaryOpType::extract(&self.previous).unwrap();
            let rhs = self.unary()?;
            return Ok(Expr::UnaryOp(UnaryOpData::new(op, Box::new(rhs))));
        }
        self.primary()
    }

    /// primary ::= NUMBER | STRING | "true" | "false" | "nil" 
    ///         ::= '(' expression ')'
    pub fn primary(&mut self) -> Result<Expr, ParserError> {
        if self.try_consume(TokenType::False) {
            return Ok(Expr::Literal(LiteralData::false_lit()));
        }
        if self.try_consume(TokenType::True) {
            return Ok(Expr::Literal(LiteralData::true_lit()));
        }
        if self.try_consume(TokenType::Nil) {
            return Ok(Expr::Literal(LiteralData::nil_lit()));
        }
        if self.try_consume(TokenType::Number) {
            let lit = self.previous.get_lexeme();
            return Ok(Expr::Literal(LiteralData::num_lit(lit)));
        }
        if self.try_consume(TokenType::String) {
            let lit = self.previous.get_lexeme();
            return Ok(Expr::Literal(LiteralData::str_lit(lit)));
        }
        if self.try_consume(TokenType::Identifier) {
            let lit = self.previous.get_lexeme();
            return Ok(Expr::Variable(VariableData::new(lit)));
        }

        if self.try_consume(TokenType::LeftParen) {
            let expr = self.expression()?;
            if !self.try_consume(TokenType::RightParen) {
                return Err(ParserError::from("No matching ')' after expression."));
            }
            return Ok(Expr::Grouping(GroupingData::new(Box::new(expr))));
        }

        Err(ParserError::from("TokenStream ended unexpectedly while parsing primary expression."))
    }

    /// Peek on the next token of the stream and consume it if it matches 
    /// the given type.
    fn try_consume(&mut self, ttype: TokenType) -> bool {
        if let Some(top) = self.toks.peek() {
            if top.get_type() == ttype {
                // peeked at next already so unwrap is ok here
                self.previous = self.toks.next().unwrap(); 
                return true;
            }
        }
        false
    }


}

#[cfg(test)] 
mod tests {
    use super::*;

    #[test]
    fn foo() {
        let src = "(5 - (3 - 1)) + -1";
        let mut parser = Parser::new(src);

    }
}
