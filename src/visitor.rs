use crate::lexer::LexemeKind;
use crate::parser::{Expr, Value};

pub trait Visitor<T> {
    fn visit_binary(&mut self, left: Expr, operator: LexemeKind, right: Expr) -> T;
    fn visit_literal(&mut self, val: Value) -> T;
    fn visit_unary(&mut self, operator: LexemeKind, right: Expr) -> T;
    fn visit_grouping(&mut self, val: Expr) -> T;
}
