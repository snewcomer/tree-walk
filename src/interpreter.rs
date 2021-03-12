use std::fmt;
use std::collections::HashMap;
use crate::parser::{Expr, Stmt, Value};
use crate::lexer::LexemeKind;
use crate::visitor::Visitor;

// Error strategy
// Lexer - captures all tokens. UNEXPECTED(String) enum variant for unknown
// Parser - add Expr::Error { line, message } if come across something that is unexpected. No build
// compile time error thrown.  Baked into return type
// Interpreter - RuntimeError when iterating over ast provided by Parser
#[derive(Debug, PartialEq)]
pub struct RuntimeError {
    line: usize,
    message: String,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} [line: {}]", self.message, self.line)
    }
}

type InterpreterResult = Result<Value, RuntimeError>;

pub struct Interpreter;

impl Interpreter {
    pub fn start(&mut self, ast: &Stmt) -> InterpreterResult {
        let mut variables = HashMap::new();

        // TODO: make a loop
        match &ast {
            Stmt::Expr(expr) => self.evaluate(expr),
            Stmt::Print(st) => Ok(Value::STRING(st.to_string())),
            Stmt::VariableDef { ident, expr } => {
                if let Some(expr) = expr {
                    // var a = expr;
                    let res = self.evaluate(expr);
                    variables.insert(ident, res);
                } else {
                    // var a;
                    variables.insert(ident, Ok(Value::Null));
                }

                Ok(Value::STRING(format!("var: {}", ident)))
            }
            Stmt::Error { line, message } => {
                Err(RuntimeError { line: *line, message: message.clone() })
            }
        }
    }

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
                message: "Can only prefix a number with + or -".to_string(),
            })
        }
    }

    fn visit_grouping(&mut self, expr: &Expr) -> InterpreterResult {
        // Ok(expr.accept(self))
        self.evaluate(expr)
    }

    fn visit_error(&mut self, line: &usize, message: &str) -> InterpreterResult {
        Err(RuntimeError {
            line: *line,
            message: message.to_string(),
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
        let mut stmts = Parser::new(tokens).parse().into_iter();
        while let Some(stmt) = stmts.next() {
            let res = Interpreter.start(&stmt.unwrap());
            assert_eq!(res.unwrap(), Value::NUMBER(-1.0));
        }
    }

    #[test]
    fn it_adds() {
        let tokens = Scanner::new("-1+1".to_owned()).collect();
        let mut stmts = Parser::new(tokens).parse().into_iter();
        while let Some(stmt) = stmts.next() {
            let res = Interpreter.start(&stmt.unwrap());
            assert_eq!(res.unwrap(), Value::NUMBER(0.0));
        }
    }

    #[test]
    fn it_unary_works() {
        let tokens = Scanner::new("+1".to_owned()).collect();
        let mut stmts = Parser::new(tokens).parse().into_iter();
        while let Some(stmt) = stmts.next() {
            let res = Interpreter.start(&stmt.unwrap());
            assert_eq!(res.unwrap(), Value::NUMBER(1.0));
        }
    }

    #[test]
    fn it_errors() {
        let tokens = Scanner::new("()".to_owned()).collect();
        let stmts = Parser::new(tokens).parse();
        let res = Interpreter.start(&stmts[0].as_ref().unwrap());
        assert_eq!(res, Ok(Value::STRING("".to_string())));
    }

    #[test]
    fn it_errors_not_number() {
        let tokens = Scanner::new("*1".to_owned()).collect();
        let stmts = Parser::new(tokens).parse();
        let res = Interpreter.start(&stmts[0].as_ref().unwrap());
        assert_eq!(res, Err(RuntimeError { line: 0, message: "Parsing error at Star".to_string() }));
        let res = Interpreter.start(&stmts[1].as_ref().unwrap());
        assert_eq!(res, Ok(Value::NUMBER(1.0)));
    }

    #[test]
    fn it_errors_invalid_operator() {
        let tokens = Scanner::new("1&1".to_owned()).collect();
        let stmt = Parser::new(tokens).parse().into_iter().nth(0).unwrap();
        let res = Interpreter.start(&stmt.unwrap());
        assert_eq!(res, Err(RuntimeError { line: 0, message: "Parsing error at &".to_string() }));
    }

    #[test]
    fn it_works_stmts() {
        let tokens = Scanner::new("print(\"foo\")".to_owned()).collect();
        let stmt = Parser::new(tokens).parse().into_iter().nth(0).unwrap();
        let res = Interpreter.start(&stmt.unwrap());
        assert_eq!(res, Ok(Value::STRING("foo".to_string())));

        let tokens = Scanner::new("print(2)".to_owned()).collect();
        let stmt = Parser::new(tokens).parse().into_iter().nth(0).unwrap();
        let res = Interpreter.start(&stmt.unwrap());
        assert_eq!(res, Ok(Value::STRING("2".to_string())));

        let tokens = Scanner::new("print(2+1)".to_owned()).collect();
        let stmt = Parser::new(tokens).parse().into_iter().nth(0).unwrap();
        let res = Interpreter.start(&stmt.unwrap());
        assert_eq!(res, Ok(Value::STRING("3".to_string())));

        let tokens = Scanner::new("print()".to_owned()).collect();
        let stmt = Parser::new(tokens).parse().into_iter().nth(0).unwrap();
        let res = Interpreter.start(&stmt.unwrap());
        assert_eq!(res, Ok(Value::STRING("".to_string())));

        let tokens = Scanner::new("var a;".to_owned()).collect();
        let stmt = Parser::new(tokens).parse().into_iter().nth(0).unwrap();
        let res = Interpreter.start(&stmt.unwrap());
        assert_eq!(res, Ok(Value::STRING("var: a".to_string())));

        let tokens = Scanner::new("var a = \"foo\";".to_owned()).collect();
        let stmt = Parser::new(tokens).parse().into_iter().nth(0).unwrap();
        let res = Interpreter.start(&stmt.unwrap());
        assert_eq!(res, Ok(Value::STRING("var: a".to_string())));
    }
}
