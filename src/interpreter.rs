use crate::parser::{Expr, Value};
use crate::lexer::LexemeKind;
use crate::visitor::{Visitor, RuntimeError};

pub struct Interpreter;

impl Interpreter {
    pub fn evaluate(self, expr: Expr<f64>) -> Result<f64, RuntimeError> {
        expr.accept(&mut self)
    }
}

impl Visitor<f64> for Interpreter {
    fn visit_binary(&mut self, l: Expr<f64>, op: LexemeKind, r: Expr<f64>) -> Result<f64, RuntimeError> {
        let num = l.accept(self)?;
        let num2 = r.accept(self)?;

        match op {
            LexemeKind::Minus => Ok(num - num2),
            LexemeKind::Plus => Ok(num + num2),
            LexemeKind::Slash => Ok(num / num2),
            LexemeKind::Star => Ok(num * num2),
            _ => Err(RuntimeError {
                line: 0,
                message: "Invalid".to_string(),
            })
        }
    }

    fn visit_literal(&mut self, val: Value<f64>) -> Result<f64, RuntimeError> {
        Ok(*val)
    }

    fn visit_unary(&mut self, op: LexemeKind, r: Expr<f64>) -> Result<f64, RuntimeError> {
        let num = r.accept(self)?;

        match op {
            LexemeKind::Minus => Ok(-num),
            LexemeKind::Plus => Ok(num),
        }
    }

    fn visit_grouping(&mut self, expr: Expr<f64>) -> Result<f64, RuntimeError> {
        expr.accept(self)
    }
}
