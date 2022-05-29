#![allow(dead_code)]
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

impl PartialEq for LiteralData {
    fn eq(&self, other: &Self) -> bool {
        (self.typ == other.typ) && (self.lexeme() == other.lexeme())
    }
}

#[inline]
pub fn make_literal(typ: LoxType, lex: &str) -> Expr {
    Expr::Literal(LiteralData::new(typ, String::from(lex)))
}

#[inline]
pub fn make_true_literal() -> Expr {
    Expr::Literal(LiteralData::true_lit())
}

#[inline]
pub fn make_false_literal() -> Expr {
    Expr::Literal(LiteralData::false_lit())
}

#[inline]
pub fn make_str_literal(lex: &str) -> Expr {
    Expr::Literal(LiteralData::str_lit(String::from(lex)))
}

#[inline]
pub fn make_num_literal(lex: &str) -> Expr {
    Expr::Literal(LiteralData::num_lit(String::from(lex)))
}

#[inline]
pub fn make_nil_literal() -> Expr {
    Expr::Literal(LiteralData::nil_lit())
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

impl PartialEq for GroupingData {
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}

#[inline]
pub fn make_grouping(inner: Expr) -> Expr {
    Expr::Grouping(GroupingData::new(Box::new(inner)))
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

impl PartialEq for UnaryOpData {
    fn eq(&self, other: &Self) -> bool {
        self.op_type() == other.op_type() && self.expr() == other.expr()
    }
}

#[inline]
pub fn make_unaryop(op: UnaryOpType, expr: Expr) -> Expr {
    Expr::UnaryOp(UnaryOpData::new(op, Box::new(expr)))
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

impl PartialEq for BinaryOpData {
    fn eq(&self, other: &Self) -> bool {
        self.op_type() == other.op_type() 
        && self.lhs() == other.lhs()
        && self.rhs() == other.rhs()
    }
}

#[inline]
pub fn make_binaryop(op: BinaryOpType, lhs: Expr, rhs: Expr) -> Expr {
    Expr::BinaryOp(BinaryOpData::new(op, Box::new(lhs), Box::new(rhs)))
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

impl PartialEq for VariableData {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() 
    }
}

#[inline]
pub fn make_variable(name: &str) -> Expr {
    Expr::Variable(VariableData::new(String::from(name)))
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

impl PartialEq for AssignData {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() 
        && self.expr() == other.expr()
    }
}

#[inline]
pub fn make_assign(name: &str, expr: Expr) -> Expr {
    Expr::Assign(AssignData::new(String::from(name), Box::new(expr)))
}

#[derive(Debug)]
pub struct LogicalData {
    op: BinaryOpType,
    lhs: Box<Expr>,
    rhs: Box<Expr>,
}

impl LogicalData {
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

impl PartialEq for LogicalData {
    fn eq(&self, other: &Self) -> bool {
        self.op_type() == other.op_type() 
        && self.lhs() == other.lhs()
        && self.rhs() == other.rhs()
    }
}

#[inline]
pub fn make_logical(op: BinaryOpType, lhs: Expr, rhs: Expr) -> Expr {
    Expr::BinaryOp(BinaryOpData::new(op, Box::new(lhs), Box::new(rhs)))
}

#[inline]
pub fn make_and(lhs: Expr, rhs: Expr) -> Expr {
    make_logical(BinaryOpType::And, lhs, rhs)
}

#[inline]
pub fn make_or(lhs: Expr, rhs: Expr) -> Expr {
    make_logical(BinaryOpType::Or, lhs, rhs)
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(LiteralData),
    Grouping(GroupingData),
    UnaryOp(UnaryOpData),
    BinaryOp(BinaryOpData),
    Variable(VariableData),
    Assign(AssignData),
    Logical(LogicalData),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Literal(ld) => write!(f, "'{}'", ld.lexeme()),
            Expr::Grouping(gd) => write!(f, "({})", gd.inner()),
            Expr::UnaryOp(od) => write!(f, "({} {})", od.op_type(), od.expr()),
            Expr::BinaryOp(od) => write!(f, "({} {} {})", od.op_type(), od.lhs(), od.rhs()),
            Expr::Variable(vd) => write!(f, "{}", vd.name()),
            Expr::Assign(ad) => write!(f, "(assign {} {})", ad.name(), ad.expr()),
            Expr::Logical(ld) => write!(f, "({} {} {})", ld.op_type(), ld.lhs(), ld.rhs()),
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
        Expr::Logical(_) => String::from("Logical"),
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

fn cmp_seq<T: PartialEq>(xs: &[T], ys: &[T]) -> bool {
    if xs.len() != ys.len() {
        return false;
    }
    
    for (x, y) in xs.iter().zip(ys.iter()) {
        if x != y {
            return false;
        }
    }
    true
}

impl PartialEq for BlockData {
    fn eq(&self, other: &Self) -> bool {
        cmp_seq(self.statements(), other.statements())
    }
}

#[inline]
pub fn make_block(stmts: Vec<Stmt>) -> Stmt {
    Stmt::Block(BlockData::new(stmts))
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

impl PartialEq for ExprData {
    fn eq(&self, other: &Self) -> bool {
        self.expr() == other.expr()
    }
}

#[inline]
pub fn make_expr(expr: Expr) -> Stmt {
    Stmt::Expr(ExprData::new(Box::new(expr)))
}

#[derive(Debug)]
pub struct FunctionData {
    name: String,
    params: Vec<String>,
    body: Vec<Stmt>,
}

impl FunctionData {
    pub fn new(name: String, params: Vec<String>, body: Vec<Stmt>) -> Self {
        Self {name: name, params: params, body: body}
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

impl PartialEq for FunctionData {
    fn eq(&self, other: &Self) -> bool {
        if self.name() != other.name() {
            return false;
        }
        if !cmp_seq(self.params(), other.params()) {
            return false;
        }
        cmp_seq(self.body(), other.body())
    }
}

#[inline]
pub fn make_function(name: &str, params: Vec<String>, body: Vec<Stmt>) -> Stmt {
    Stmt::Function(FunctionData::new(String::from(name), params, body))
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

impl PartialEq for ClassData {
    fn eq(&self, other: &Self) -> bool {
        if self.name() != other.name() {
            return false;
        }
        if self.super_class() != other.super_class() {
            return false;
        }
        cmp_seq(self.functions(), other.functions())
    }
}

#[inline]
pub fn make_class(name: &str, supercl: VariableData, fns: Vec<FunctionData>) -> Stmt {
    Stmt::Class(ClassData::new(String::from(name), Box::new(supercl), fns))
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

impl PartialEq for IfData {
    fn eq(&self, other: &Self) -> bool {
        self.condition() == other.condition() && 
        self.then_branch() == other.then_branch() &&
        self.else_branch() == other.else_branch()
    }
}

#[inline]
pub fn make_if(cond: Expr, thenbr: Stmt, elsebr: Option<Stmt>) -> Stmt {
    let mut elopt: Option<Box<Stmt>> = None;
    if let Some(elsebr_val) = elsebr {
        elopt = Some(Box::new(elsebr_val));
    }
    Stmt::If(IfData::new(Box::new(cond), Box::new(thenbr), elopt))
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

impl PartialEq for PrintData {
    fn eq(&self, other: &Self) -> bool {
        self.expr() == other.expr()
    }
}

#[inline]
pub fn make_print(expr: Expr) -> Stmt {
    Stmt::Print(PrintData::new(Box::new(expr)))
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

impl PartialEq for ReturnData {
    fn eq(&self, other: &Self) -> bool {
        self.keyword() == other.keyword() &&
        self.value() == other.value()
    }
}

#[inline]
pub fn make_return(keyword: &str, value: Expr) -> Stmt {
    Stmt::Return(ReturnData::new(String::from(keyword), Box::new(value)))
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

    pub fn has_initializer(&self) -> bool {
        !self.initializer().is_none()
    }

    pub fn initializer(&self) -> Option<&Expr> {
        match &self.initializer {
           Some(expr) => Some(&expr),
           None => None, 
        }
    }
}

impl PartialEq for VarDeclData {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name() &&
        self.initializer() == other.initializer()
    }
}

#[inline]
pub fn make_vardecl(name: &str, init: Option<Expr>) -> Stmt {
    let mut initop: Option<Box<Expr>> = None;
    if let Some(init_val) = init {
        initop = Some(Box::new(init_val));
    }
    Stmt::VarDecl(VarDeclData::new(String::from(name), initop))
}

#[derive(Debug)]
pub struct WhileData {
    condition: Box<Expr>, 
    body: Vec<Stmt>,
}

impl WhileData {
    pub fn new(cond: Box<Expr>, body: Vec<Stmt>) -> Self {
        Self {condition: cond, body: body}
    }

    pub fn condition(&self) -> &Expr {
        &self.condition
    }

    pub fn body(&self) -> &[Stmt] {
        &self.body
    }
}

impl PartialEq for WhileData {
    fn eq(&self, other: &Self) -> bool {
        if self.condition() != other.condition() {
            return false;
        }
        cmp_seq(self.body(), other.body())
    }
}

pub fn make_while(cond: Expr, body: Vec<Stmt>) -> Stmt {
    Stmt::While(WhileData::new(Box::new(cond), body))
}

#[derive(Debug, PartialEq)]
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