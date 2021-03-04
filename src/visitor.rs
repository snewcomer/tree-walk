use std::error;
use std::fmt;
use crate::lexer::LexemeKind;
use crate::parser::{Expr, Value};

#[derive(Debug)]
pub struct RuntimeError {
    line: u64,
    message: String,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} [line: {}]", self.message, self.line)
    }
}

impl error::Error for RuntimeError {}


pub trait Visitor<T> {
    fn visit_binary(&mut self, left: Expr<T>, operator: LexemeKind, right: Expr<T>) -> Result<T, RuntimeError>;
    fn visit_literal(&mut self, val: Value<T>) -> Result<T, RuntimeError>;
    fn visit_unary(&mut self, operator: LexemeKind, right: Expr<T>) -> Result<T, RuntimeError>;
    fn visit_grouping(&mut self, val: Expr<T>) -> Result<T, RuntimeError>;
}
