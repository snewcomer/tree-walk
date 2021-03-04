use std::error;
use std::fmt;
use crate::parser::{Expr, Value};
use crate::lexer::LexemeKind;
use crate::visitor::Visitor;

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

type InterpreterResult = Result<Value, RuntimeError>;

pub struct Interpreter;

impl Interpreter {
    pub fn evaluate(self, expr: Expr) -> InterpreterResult {
        expr.accept(&mut self)
    }
}

impl Visitor<InterpreterResult> for Interpreter {
    fn visit_binary(&mut self, l: Expr, op: LexemeKind, r: Expr) -> InterpreterResult {
        let num = *l.accept(self)?;
        let num2 = *r.accept(self)?;

        match op {
            LexemeKind::Minus => Ok(Value::NUMBER(num - num2)),
            LexemeKind::Plus => Ok(Value::NUMBER(num + num2)),
            LexemeKind::Slash => Ok(Value::NUMBER(num / num2)),
            LexemeKind::Star => Ok(Value::NUMBER(num * num2)),
            _ => Err(RuntimeError {
                line: 0,
                message: "Invalid".to_string(),
            })
        }
    }

    fn visit_literal(&mut self, val: Value) -> InterpreterResult {
        Ok(val)
    }

    fn visit_unary(&mut self, op: LexemeKind, r: Expr) -> InterpreterResult {
        let num = r.accept(self)?;

        match op {
            LexemeKind::Minus => Ok(Value::NUMBER(-num)),
            LexemeKind::Plus => Ok(Value::NUMBER(num)),
        }
    }

    fn visit_grouping(&mut self, expr: Expr) -> InterpreterResult {
        Ok(expr.accept(self))
    }
}
