#![allow(dead_code)]
use super::super::lexer::token::{Token, TokenType};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOpType {
    Not,  
    Neg,
}

impl UnaryOpType {
    pub fn extract(tok: &Token) -> Option<UnaryOpType> {
        match tok.get_type() {
            TokenType::Bang => Some(UnaryOpType::Not),
            TokenType::Minus => Some(UnaryOpType::Neg),
            _ => None
        }
    }
}

impl fmt::Display for UnaryOpType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
        match self {
            UnaryOpType::Not => write!(f, "!"),
            UnaryOpType::Neg => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOpType {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Add, 
    Sub,
    Mult,
    Div,
    And,
    Or,
}

impl BinaryOpType {
    pub fn extract(tok: &Token) -> Option<BinaryOpType> {
        match tok.get_type() {
            TokenType::EqualEqual => Some(BinaryOpType::Equal),
            TokenType::BangEqual => Some(BinaryOpType::NotEqual),
            TokenType::Less => Some(BinaryOpType::Less),
            TokenType::LessEqual => Some(BinaryOpType::LessEqual),
            TokenType::Greater => Some(BinaryOpType::Greater),
            TokenType::GreaterEqual => Some(BinaryOpType::GreaterEqual),
            TokenType::Plus => Some(BinaryOpType::Add),
            TokenType::Minus => Some(BinaryOpType::Sub),
            TokenType::Star => Some(BinaryOpType::Mult),
            TokenType::Slash => Some(BinaryOpType::Div),
            TokenType::And => Some(BinaryOpType::And),
            TokenType::Or => Some(BinaryOpType::Or),
            _ => None
        }
    }
}

impl fmt::Display for BinaryOpType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
        match self {
            BinaryOpType::Equal => write!(f, "=="),
            BinaryOpType::NotEqual => write!(f, "!="),
            BinaryOpType::Less => write!(f, "<"),
            BinaryOpType::LessEqual => write!(f, "<="),
            BinaryOpType::Greater => write!(f, ">"),
            BinaryOpType::GreaterEqual => write!(f, ">="),
            BinaryOpType::Add => write!(f, "+"),
            BinaryOpType::Sub => write!(f, "-"),
            BinaryOpType::Mult => write!(f, "*"),
            BinaryOpType::Div => write!(f, "/"),
            BinaryOpType::And => write!(f, "and"),
            BinaryOpType::Or => write!(f, "or"),
        }
    }                     
}                         