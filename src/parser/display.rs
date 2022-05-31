use super::ast::*;
use std::fmt;

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
            Expr::Call(cd) => write!(f, "({}({:?})", cd.callee(), cd.args()),
        }
    }
}

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

impl fmt::Display for WhileData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Stmt::While: -- start --\n");
        write!(f, "-- condition --\n{}\n", self.condition());
        write!(f, "-- body --\n{}\n", self.body());
        write!(f, "Stmt::While: -- end --")
    }
}

impl fmt::Display for ForData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Stmt::While: -- start --\n");
        if let Some(i) = self.initializer() {
            write!(f, "-- initializer --\n{}\n", i);
        }
        if let Some(c) = self.condition() {
            write!(f, "-- condition --\n{}\n", c);
        }
        if let Some(r) = self.incrementor() {
            write!(f, "-- increment --\n{}\n", r);
        }
        write!(f, "-- body --\n{}\n", self.body());
        write!(f, "Stmt::While: -- end --")
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
            Stmt::While(wd) => write!(f, "{}", wd),
            Stmt::For(fd) => write!(f, "{}", fd),
            _ => write!(f, "...."),
        }
    }
}
