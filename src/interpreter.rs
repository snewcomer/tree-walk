use crate::parser::{Expr, Value};
use crate::lexer::LexemeKind;
use crate::visitor::Visitor;

pub struct Interpreter;

impl Interpreter {
    pub fn evaluate(&self, expr: Expr) -> f64 {
        expr.accept(&mut self)
    }
}

impl Visitor<f64> for Interpreter {
    fn visit_binary(&mut self, l: Expr, op: LexemeKind, r: Expr) -> f64 {
        let num = l.accept(self);
        let num2 = r.accept(self);

        match op {
            LexemeKind::Minus => num - num2,
            LexemeKind::Plus => num + num2,
            LexemeKind::Slash => num / num2,
        }
    }

    fn visit_literal(&mut self, val: Value) -> f64 {
        val
    }

    fn visit_unary(&mut self, op: LexemeKind, r: Expr) -> f64 {
        let num = r.accept(self);

        match op {
            LexemeKind::Minus => -num,
            LexemeKind::Plus => num,
        }
    }

    fn visit_grouping(&mut self, val: Expr) -> f64 {
        val.accept(self)
    }
}
