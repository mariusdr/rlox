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
pub struct AssignData {
    name: String,
    expr: Box<Expr>,
}

impl AssignData {
    pub fn new(name: String, expr: Box<Expr>) -> Self {
        Self {name: name, expr: expr}
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug)]
pub enum Expr {
    Literal(LiteralData),
    Grouping(GroupingData),
    UnaryOp(UnaryOpData),
    BinaryOp(BinaryOpData),
    Variable(VariableData),
    Assign(AssignData),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal(ld) => write!(f, "'{}'", ld.lexeme()),
            Expr::Grouping(gd) => write!(f, "({})", gd.inner()),
            Expr::UnaryOp(od) => write!(f, "({} {})", od.op_type(), od.expr()),
            Expr::BinaryOp(od) => write!(f, "({} {} {})", od.op_type(), od.lhs(), od.rhs()),
            Expr::Variable(vd) => write!(f, "{}", vd.name()),
            Expr::Assign(ad) => write!(f, "(assign {} {})", ad.name(), ad.expr())
        }
    }
}

pub fn expr_type(expr: &Expr) -> String {
    let typestr = match expr {
        Expr::Literal(_) => String::from("Literal"),
        Expr::Grouping(_) => String::from("Grouping"),
        Expr::UnaryOp(_) => String::from("UnaryOp"),
        Expr::BinaryOp(_) => String::from("BinaryOp"),
        Expr::Variable(_) => String::from("Variable"),
        Expr::Assign(_) => String::from("Assign"),
    };
    typestr 
}

#[derive(Debug)]
pub struct BlockData {
    statements: Vec<Stmt>,
}

impl BlockData {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self {statements: stmts}
    }

    pub fn statements(&self) -> &[Stmt] {
        &self.statements
    }
}

#[derive(Debug)]
pub struct ExprData {
    expr: Box<Expr>,
}

impl ExprData {
    pub fn new(expr: Box<Expr>) -> Self {
        Self {expr: expr}
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug)]
pub struct FunctionData {
    name: String,
    params: Vec<String>,
    body: Vec<Stmt>,
}

impl FunctionData {
    pub fn new(n: String, ps: Vec<String>, body: Vec<Stmt>) -> Self {
        Self {name: n, params: ps, body: body}
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn params(&self) -> &[String] {
        &self.params
    }

    pub fn body(&self) -> &[Stmt] {
        &self.body
    }
}

#[derive(Debug)]
pub struct ClassData {
    name: String,
    super_class: Box<VariableData>,
    functions: Vec<FunctionData>,
}

impl ClassData {
    pub fn new(n: String, sc: Box<VariableData>, fns: Vec<FunctionData>) -> Self {
        Self {name: n, super_class: sc, functions: fns}
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn super_class(&self) -> &VariableData {
        &self.super_class
    } 

    pub fn functions(&self) -> &[FunctionData] {
        &self.functions
    }
}

#[derive(Debug)]
pub struct IfData {
    condition: Box<Expr>,
    then_branch: Box<Stmt>,
    else_branch: Option<Box<Stmt>>,
}

impl IfData {
    pub fn new(cond: Box<Expr>, then_do: Box<Stmt>, else_do: Option<Box<Stmt>>) -> Self {
        Self {condition: cond, then_branch: then_do, else_branch: else_do}
    }

    pub fn condition(&self) -> &Expr {
        &self.condition
    }

    pub fn then_branch(&self) -> &Stmt {
        &self.then_branch
    }

    pub fn else_branch(&self) -> Option<&Stmt> {
        match &self.else_branch {
            Some(stmt) => Some(&stmt),
            None => None,
        }
    }
}

#[derive(Debug)]
pub struct PrintData {
    expr: Box<Expr>,
}

impl PrintData {
    pub fn new(expression: Box<Expr>) -> Self {
        Self {expr: expression}
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

#[derive(Debug)]
pub struct ReturnData {
    keyword: String,
    value: Box<Expr>,
}

impl ReturnData {
    pub fn new(keyword: String, value: Box<Expr>) -> Self {
        Self {keyword: keyword, value: value}
    }

    pub fn keyword(&self) -> &str {
        &self.keyword
    }

    pub fn value(&self) -> &Expr {
        &self.value
    }
}

#[derive(Debug)]
pub struct VarDeclData {
    name: String,
    initializer: Option<Box<Expr>>,
}

impl VarDeclData {
    pub fn new(name: String, initializer: Option<Box<Expr>>) -> Self {
        Self {name: name, initializer: initializer}
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn initializer(&self) -> Option<&Expr> {
        match &self.initializer {
           Some(expr) => Some(&expr),
           None => None, 
        }
    }
}

#[derive(Debug)]
pub struct WhileData {
    condition: Box<Expr>, 
    body: Box<Stmt>,
}

impl WhileData {
    pub fn new(cond: Box<Expr>, body: Box<Stmt>) -> Self {
        Self {condition: cond, body: body}
    }

    pub fn condition(&self) -> &Expr {
        &self.condition
    }

    pub fn body(&self) -> &Stmt {
        &self.body
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprData),
    VarDecl(VarDeclData),
    Block(BlockData),
    Function(FunctionData),
    Class(ClassData),
    Return(ReturnData),
    If(IfData),
    While(WhileData),
    Print(PrintData),
}

pub type Stmts = Vec<Stmt>;


impl fmt::Display for ExprData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Stmt::Expr: {}", self.expr())
    }
}

impl fmt::Display for VarDeclData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.initializer() {
            Some(expr) => write!(f, "Stmt::VarDecl: {} <- {}", self.name(), expr),
            None => write!(f, "Stmt::VarDecl: {}", self.name()),
        }
    }
}

impl fmt::Display for PrintData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Stmt::Print: {}", self.expr())
    }
}

impl fmt::Display for BlockData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Stmt::Block: -- start --\n");
        for s in self.statements() {
            write!(f, "{}\n", s);
        }
        write!(f, "Stmt::Block: -- end --")
    }
}

impl fmt::Display for IfData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Stmt::If: -- start --\n");
        write!(f, "-- condition --\n{}\n", self.condition());
        write!(f, "-- then --\n{}\n", self.then_branch());
        if let Some(stmt) = self.else_branch() {
            write!(f, "-- else --\n{}\n", stmt);
        }
        write!(f, "Stmt::If: -- end --")
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Expr(ed) => write!(f, "{}", ed),
            Stmt::Print(pd) => write!(f, "{}", pd),
            Stmt::VarDecl(vd) => write!(f, "{}", vd),
            Stmt::Block(bd) => write!(f, "{}", bd),
            Stmt::If(id) => write!(f, "{}", id),
            _ => write!(f, "...."),
        }
    }
}