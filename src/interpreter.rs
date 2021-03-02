use std::error;
use std::fmt;
use crate::parser::{Expr, Value};
use crate::lexer::LexemeKind;
use crate::visitor::Visitor;

#[derive(Debug, PartialEq)]
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
    pub fn evaluate(&mut self, expr: &Expr) -> InterpreterResult {
        expr.accept(self)
    }
}

impl Visitor<InterpreterResult> for Interpreter {
    fn visit_binary(&mut self, l: &Expr, op: &LexemeKind, r: &Expr) -> InterpreterResult {
        let num = unwrap_number(self.evaluate(l))?;
        let num2 = unwrap_number(self.evaluate(r))?;

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

    fn visit_literal(&mut self, val: &Value) -> InterpreterResult {
        Ok(val.clone())
    }

    fn visit_unary(&mut self, op: &LexemeKind, r: &Expr) -> InterpreterResult {
        let num = unwrap_number(self.evaluate(r))?;

        match op {
            LexemeKind::Minus => Ok(Value::NUMBER(-num)),
            LexemeKind::Plus => Ok(Value::NUMBER(num)),
            _ => Err(RuntimeError {
                line: 0,
                message: "Not the right op code".to_string(),
            })
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) -> InterpreterResult {
        // Ok(expr.accept(&mut self))
        self.evaluate(expr)
    }

    fn visit_error(&mut self) -> InterpreterResult {
        Err(RuntimeError {
            line: 0,
            message: "General Error".to_string(),
        })
    }
}

fn unwrap_number(v: InterpreterResult) -> Result<f64, RuntimeError> {
    match v {
        Ok(Value::NUMBER(n)) => Ok(n),
        _ => Err(RuntimeError {
            line: 0,
            message: "Not a number".to_string(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Scanner;
    use crate::parser::Parser;

    #[test]
    fn it_works() {
        let tokens = Scanner::new("-1".to_owned()).collect();
        let ast = Parser::new(tokens).parse().unwrap();
        let res = Interpreter.evaluate(&ast);
        assert_eq!(res.unwrap(), Value::NUMBER(-1.0));
    }

    #[test]
    fn it_errors() {
        let tokens = Scanner::new("()".to_owned()).collect();
        let ast = Parser::new(tokens).parse().unwrap();
        let res = Interpreter.evaluate(&ast);
        assert_eq!(res, Err(RuntimeError { line: 0, message: "General Error".to_string() }));
    }
}
