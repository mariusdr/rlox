#![allow(dead_code)]
pub mod ast;
pub mod optypes;
mod display;
mod tests;
mod loxtypes;

use std::result::Result;
use std::iter::Peekable;
use std::fmt::{Write};
use std::f64;

use ast::*;
use optypes::*;
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
        self.assignment()
    }

    #[inline]
    fn assignment_error(&self, expr: &Expr) -> Result<Expr, ParserError> {
        let mut buf = String::new();
        write!(&mut buf, "An expression of type '{}' is a invalid assignment target.", expr_type(&expr));
        Err(ParserError::new(buf))
    }

    /// assignment ::= variable '=' assignment 
    ///            ::= or
    pub fn assignment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.or()?;

        if self.try_consume(TokenType::Equal) {
            let value = self.assignment()?;
            if let Expr::Variable(vd) = expr {
                return Ok(make_assign(vd.name(), value));
            } 
            return self.assignment_error(&expr);
        }
        Ok(expr)
    }

    /// and ::= equality ( 'and' equality )*
    fn and(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.equality()?;
        while self.try_consume(TokenType::And) {
            let rhs = self.equality()?;
            lhs = make_and(lhs, rhs);
        }
        Ok(lhs)
    }

    /// or ::= and ( 'or' and )*
    fn or(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.and()?;
        while self.try_consume(TokenType::Or) {
            let rhs = self.and()?;
            lhs = make_or(lhs, rhs);
        }
        Ok(lhs)
    }

    /// retrieve binary op type of previous token.
    fn binop_type(&mut self) -> Result<BinaryOpType, ParserError> {
        if let Some(op) = BinaryOpType::extract(&self.previous) {
            return Ok(op);
        }
        Err(ParserError::from("Previous token was not a binary operator."))
    }

    /// equality ::= comparison ('!=' | '==') comparison | comparison
    pub fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.comparison()?;

        while self.try_consume(TokenType::BangEqual) || self.try_consume(TokenType::EqualEqual) {
            let op = self.binop_type()?;
            let rhs = self.comparison()?;
            lhs = make_binaryop(op, lhs, rhs);
        }
        Ok(lhs)
    }

    /// comparison ::= term ('<' | '<=' | '>' | '>=') term
    pub fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.term()?;

        while self.try_consume(TokenType::Greater) || self.try_consume(TokenType::GreaterEqual) ||
              self.try_consume(TokenType::Less) || self.try_consume(TokenType::LessEqual) 
        {
            let op = self.binop_type()?;
            let rhs = self.term()?;
            lhs = make_binaryop(op, lhs, rhs);
        }
        Ok(lhs)
    }

    /// term ::= factor ('+' | '-') factor | factor
    pub fn term(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.factor()?;

        while self.try_consume(TokenType::Minus) || self.try_consume(TokenType::Plus) {
            let op = self.binop_type()?;
            let rhs = self.factor()?;
            lhs = make_binaryop(op, lhs, rhs);
        }
        Ok(lhs)
    }

    /// factor ::= unary ('/' | '*') factor | unary
    pub fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut lhs = self.unary()?;
        
        while self.try_consume(TokenType::Slash) || self.try_consume(TokenType::Star) {
            let op = self.binop_type()?;
            let rhs = self.factor()?;
            lhs = make_binaryop(op, lhs, rhs);
        }
        Ok(lhs)
    }

    /// retrieve unary op type of previous token.
    fn unaryop_type(&mut self) -> Result<UnaryOpType, ParserError> {
        if let Some(op) = UnaryOpType::extract(&self.previous) {
            return Ok(op);
        }
        Err(ParserError::from("Previous token was not a unary operator."))
    }

    /// unary ::= '-' unary | '!' unary
    ///       ::= primary
    pub fn unary(&mut self) -> Result<Expr, ParserError> {
        if self.try_consume(TokenType::Bang) | self.try_consume(TokenType::Minus) {
            let op = self.unaryop_type()?;
            let rhs = self.unary()?;
            return Ok(make_unaryop(op, rhs));
        }
        self.primary()
    }

    /// primary ::= NUMBER | STRING | "true" | "false" | "nil" 
    ///         ::= '(' expression ')'
    pub fn primary(&mut self) -> Result<Expr, ParserError> {
        if self.try_consume(TokenType::False) {
            return Ok(make_false_literal());
        }
        if self.try_consume(TokenType::True) {
            return Ok(make_true_literal());
        }
        if self.try_consume(TokenType::Nil) {
            return Ok(make_nil_literal());
        }
        if self.try_consume(TokenType::Number) {
            let lit = &self.previous.get_lexeme();
            return Ok(make_num_literal(lit));
        }
        if self.try_consume(TokenType::String) {
            let lit = &self.previous.get_lexeme();
            return Ok(make_str_literal(lit));
        }
        if self.try_consume(TokenType::Identifier) {
            let lit = &self.previous.get_lexeme();
            return Ok(make_variable(lit));
        }

        if self.try_consume(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.require(TokenType::RightParen, "Missing matching ')'.")?;
            return Ok(make_grouping(expr));
        }

        Err(ParserError::from("TokenStream ended unexpectedly while parsing primary expression."))
    }

    /// printstmt ::= 'print' expression ';'
    fn print_statement(&mut self) -> Result<Stmt, ParserError> {
        let value = self.expression()?;
        self.require(TokenType::Semicolon, "Missing ';' after statement")?;
        Ok(make_print(value))
    }

    /// exprstmt ::= expression ';'
    fn expr_statement(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        self.require(TokenType::Semicolon, "Missing ';' after statement")?;
        Ok(make_expr(expr))
    }

    /// statement ::= printstmt 
    ///           ::= exrpstmt
    ///           ::= '{' blockstmt
    ///           ::= 'if' ifstmt
    ///           ::= 'for' forstmt
    pub fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.try_consume(TokenType::Print) {
            return self.print_statement();
        }
        if self.try_consume(TokenType::LeftBrace) {
            return self.block_statement();
        }
        if self.try_consume(TokenType::If) {
            return self.if_statement();
        }
        if self.try_consume(TokenType::While) {
            return self.while_statement();
        }
        if self.try_consume(TokenType::For) {
            return self.for_statement();
        }
        self.expr_statement()
    }

    /// vardeclstmt ::= IDENTIFIER ('=' expression)? ';'
    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        self.require(TokenType::Identifier, "Expected identifier in var declaration.")?;

        if !self.previous.has_lexeme() {
            return Err(ParserError::from("Var declaration with empty name."));
        } 
        let name = &self.previous.get_lexeme();

        let mut initializer: Option<Expr> = None;
        if self.try_consume(TokenType::Equal) {
            let expr = self.expression()?;
            initializer = Some(expr);
        }
        self.require(TokenType::Semicolon, "Missing ';' after statement")?;
        Ok(make_vardecl(name, initializer))
    }

    /// declstmt ::= 'var' vardeclstmt
    ///          ::= statement
    pub fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self.try_consume(TokenType::Var) {
            return self.var_declaration();
        }
        // TODO: error recovery should be done here!
        self.statement()
    }

    /// blockstmt ::= declstmt* '}'
    pub fn block_statement(&mut self) -> Result<Stmt, ParserError> {
        let mut stmts = Stmts::new();
        while !self.is_done() && !self.peek_at_next_token(TokenType::RightBrace) {
            stmts.push(self.declaration()?)
        }
        self.require(TokenType::RightBrace, "Expected '}' after block.")?;
        Ok(make_block(stmts))
    }

    /// ifstmt ::= '(' expression ')' statement ('else' statement)?
    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.require(TokenType::LeftParen, "Expected '(' after 'if'.")?;
        let cond = self.expression()?;
        self.require(TokenType::RightParen, "Expected ')' after 'if' condition.")?;

        let then_br = self.statement()?;
        let mut else_br: Option<Stmt> = None;
        if self.try_consume(TokenType::Else) {
            else_br = Some(self.statement()?);
        }        
        Ok(make_if(cond, then_br, else_br))
    }

    /// whilestmt ::= 'while' '(' expression ')' statement
    fn while_statement(&mut self) -> Result<Stmt, ParserError> {
        self.require(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let cond = self.expression()?;
        self.require(TokenType::RightParen, "Expect ')' after condition.")?;

        let body = self.statement()?;
        Ok(make_while(cond, body))
    }

    /// forstmt ::= 'for' '(' (vardecl | exprstmt | ';') expression? ';' expression? ')' statement
    fn for_statement(&mut self) -> Result<Stmt, ParserError> {
        self.require(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let init: Option<Stmt>;
        if self.try_consume(TokenType::Semicolon) {
            init = None;
        } else if self.try_consume(TokenType::Var) {
            init = Some(self.var_declaration()?);
        } else {
            init = Some(self.expr_statement()?);
        }

        let cond: Option<Expr>;
        if self.try_consume(TokenType::Semicolon) {
            cond = None;
        } else {
            cond = Some(self.expression()?);
            self.require(TokenType::Semicolon, "Expect ';' after loop condition.")?;
        }

        let incr: Option<Expr>;
        if self.try_consume(TokenType::RightParen) {
            incr = None;
        } else {
            incr = Some(self.expression()?);
            self.require(TokenType::RightParen, "Expect ')' after for clause")?;
        }
 
        let body = self.statement()?;
        Ok(make_for(init, cond, incr, body))
    }

    pub fn parse(&mut self) -> Result<Stmts, ParserError> {
        let mut stmts = Stmts::new();
        while !self.is_done() {
            stmts.push(self.declaration()?);
        } 
        Ok(stmts)
    }

    /// Peek on the next token and return its type.
    fn peek_at_next_tokentype(&mut self) -> Option<TokenType> {
        if let Some(top) = self.toks.peek() {
            return Some(top.get_type());
        }
        None
    }

    /// Peek on the next token and check if it has the given type.
    fn peek_at_next_token(&mut self, ttype: TokenType) -> bool {
        if let Some(top) = self.toks.peek() {
            if top.get_type() == ttype {
                return true;
            }
        }
        false
    }

    /// Peek on the next token of the stream and consume it if it matches 
    /// the given type.
    fn try_consume(&mut self, ttype: TokenType) -> bool {
        if self.peek_at_next_token(ttype) {
            // peeked at next already so unwrap is ok here
            self.previous = self.toks.next().unwrap();
            return true;
        }
        false
    }

    /// Try to consume the next token and fail with a parser error if that 
    /// was not possible.
    fn require(&mut self, ttype: TokenType, on_error: &str) -> Result<(), ParserError> {
        if !self.try_consume(ttype) {
            return Err(ParserError::from(on_error));
        }
        Ok(())
    }

    #[inline]
    fn is_done(&mut self) -> bool {
        self.peek_at_next_token(TokenType::Eof)
    }
}
