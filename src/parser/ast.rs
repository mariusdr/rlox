use std::boxed::Box;
use std::fmt;

use super::super::lexer::token::{Token, TokenType};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOpType {
    /// The ! operator for logical inversion
    Not,  
    /// The - operator for negation
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
}

impl BinaryOpType {
    pub fn extract(tok: &Token) -> Option<BinaryOpType> {
        match tok.get_type() {
            TokenType::Equal => Some(BinaryOpType::Equal),
            TokenType::BangEqual => Some(BinaryOpType::NotEqual),
            TokenType::Less => Some(BinaryOpType::Less),
            TokenType::LessEqual => Some(BinaryOpType::LessEqual),
            TokenType::Greater => Some(BinaryOpType::Greater),
            TokenType::GreaterEqual => Some(BinaryOpType::GreaterEqual),
            TokenType::Plus => Some(BinaryOpType::Add),
            TokenType::Minus => Some(BinaryOpType::Sub),
            TokenType::Star => Some(BinaryOpType::Mult),
            TokenType::Slash => Some(BinaryOpType::Div),
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
        }                 
    }                     
}                         

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LoxType {
    String,
    Number,
    Boolean,
    Nil,
}

impl fmt::Display for LoxType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let out = match self {
            LoxType::String => "String",
            LoxType::Number => "Number",
            LoxType::Boolean => "Boolean",
            LoxType::Nil => "Nil",
        };
        write!(f, "{}", out)
    }
}

#[derive(Debug)]
pub struct LiteralData {
    typ: LoxType,
    lexeme: String,
}

impl LiteralData {
    pub fn new(typ: LoxType, lex: String) -> Self {
        Self {typ: typ, lexeme: lex}
    }
    
    pub fn lox_type(&self) -> LoxType {
        self.typ
    }    

    pub fn lexeme(&self) -> &str {
        self.lexeme.as_ref()
    }

    pub fn true_lit() -> Self {
        Self::new(LoxType::Boolean, String::from("true"))
    }

    pub fn false_lit() -> Self {
        Self::new(LoxType::Boolean, String::from("false"))
    }

    pub fn str_lit(lex: String) -> Self {
        Self::new(LoxType::String, lex)
    }

    pub fn num_lit(lex: String) -> Self {
        Self::new(LoxType::Number, lex)
    }

    pub fn nil_lit() -> Self {
        Self::new(LoxType::Nil, String::new())
    }
}

#[derive(Debug)]
pub struct GroupingData {
    inner: Box<Expr>,
}

impl GroupingData {
    pub fn new(inner: Box<Expr>) -> Self {
        Self {inner: inner}
    }

    pub fn inner(&self) -> &Expr {
        &self.inner
    }
}

#[derive(Debug)]
pub struct UnaryOpData {
    op: UnaryOpType,
    expr: Box<Expr>,
}

impl UnaryOpData {
    pub fn new(op: UnaryOpType, expr: Box<Expr>) -> Self {
        Self {op: op, expr: expr}
    }

    pub fn op_type(&self) -> UnaryOpType {
        self.op
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug)]
pub struct BinaryOpData {
    op: BinaryOpType,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

impl BinaryOpData {
    pub fn new(op: BinaryOpType, lhs: Box<Expr>, rhs: Box<Expr>) -> Self {
        Self {op: op, lhs: lhs, rhs: rhs}
    }

    pub fn op_type(&self) -> BinaryOpType {
        self.op
    }
    
    pub fn lhs(&self) -> &Expr {
        &self.lhs
    }
    
    pub fn rhs(&self) -> &Expr {
        &self.rhs
    }
}

#[derive(Debug)]
pub struct VariableData {
    name: String,
}

impl VariableData {
    pub fn new(name: String) -> Self {
        Self {name: name}
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub enum Expr {
    Literal(LiteralData),
    Grouping(GroupingData),
    UnaryOp(UnaryOpData),
    BinaryOp(BinaryOpData),
    Variable(VariableData),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal(ld) => write!(f, "'{}'", ld.lexeme()),
            Expr::Grouping(gd) => write!(f, "({})", gd.inner()),
            Expr::UnaryOp(od) => write!(f, "({} {})", od.op_type(), od.expr()),
            Expr::BinaryOp(od) => write!(f, "({} {} {})", od.op_type(), od.lhs(), od.rhs()),
            Expr::Variable(vd) => write!(f, "{}", vd.name()),
        }
    }
}
