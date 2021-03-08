use crate::lexer::LexemeKind;
use super::expression::{Expr, Value};
use crate::interpreter::Interpreter;
use super::Parser;

pub(crate) fn parse(p: &mut Parser) -> Option<Stmt> {
    if p.at(LexemeKind::PRINT) {
        Some(print_stmt(p))
    } else if p.at(LexemeKind::VAR) {
        Some(declaration_stmt(p))
    } else {
        let expr = p.expression().unwrap();
        Some(Stmt::Expr(expr))
    }
}

fn declaration_stmt(p: &mut Parser) -> Stmt {
    // var x = 1+1;
    p.cursor += 1;

    while p.is_equal(vec![LexemeKind::Whitespace]) {
        p.cursor += 1;
    }

    if let Some(ident) = variable_ref(p) {
        p.cursor += 1;
        if p.peek_kind() == Some(LexemeKind::Semicolon) {
            // var a;
            Stmt::VariableDef { ident, expr: None }
        } else {
            // TODO: make strict
            while p.is_equal(vec![LexemeKind::Whitespace, LexemeKind::Equal]) {
                p.cursor += 1;
            }

            if let Some(expr) = p.expression() {
                let res = Stmt::VariableDef { ident, expr: Some(expr) };

                assert!(p.at(LexemeKind::Semicolon));

                res
            } else {
                let last_token = p.last_token().unwrap();
                Stmt::Error { line: last_token.line, message: "Unfinished declaration statement".to_string() }
            }
        }
    } else {
        let last_token = p.last_token().unwrap();
        Stmt::Error { line: last_token.line, message: "Unfinished declaration statement".to_string() }
    }

}

fn variable_ref(p: &mut Parser) -> Option<String> {
    match p.peek_kind() {
        Some(LexemeKind::IDENTIFIER(st)) => {
            Some(st)
        },
        _ => None
    }
}


fn print_stmt(p: &mut Parser) -> Stmt {
    p.cursor += 1;

    let res = p.primary();
    let st = match res {
        Some(Expr::Grouping(ref bin)) => {
            let v = Interpreter.evaluate(bin).unwrap();
            let st = match v {
                Value::BOOLEAN(true) => "true".to_string(),
                Value::BOOLEAN(false) => "true".to_string(),
                Value::STRING(st) => st.to_string(),
                Value::NUMBER(n) => n.to_string(),
                Value::Null => "".to_string(),
            };
            Stmt::Print(st)
        },
        Some(Expr::Literal(Value::STRING(st))) => Stmt::Print(st),
        Some(Expr::Literal(Value::NUMBER(st))) => Stmt::Print(st.to_string()),
        Some(Expr::Error { .. }) => Stmt::Print("".to_string()), // print() or unfinished
        _ => {
            if !p.at_end() {
                let token = p.peek().unwrap();
                Stmt::Error { line: token.line, message: "Error".to_string() }
            } else {
                let last_token = p.last_token().unwrap();
                Stmt::Error { line: last_token.line, message: "Fatal error".to_string() }
            }
        }
    };

    let result = p.expect(LexemeKind::RightParen);
    match result {
        Ok(_) => st,
        _ => {
            let last_token = p.last_token().unwrap();
            Stmt::Error { line: last_token.line, message: "Unfinished print statement".to_string() }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VariableDef {
        ident: String,
        expr: Option<Expr>,
    },
    Print(String),
    Expr(Expr),
    Error {
        line: usize,
        message: String,
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Scanner;
    use crate::parser::Parser;

    #[test]
    fn it_works() {
        let tokens = Scanner::new("print(1)".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(res, Some(Stmt::Print("1".to_string())));
    }

    #[test]
    fn it_accepts_nothing() {
        let tokens = Scanner::new("print()".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(res, Some(Stmt::Print("".to_string())));
    }

    #[test]
    fn it_accepts_expressions() {
        let tokens = Scanner::new("print(8*8)".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(res, Some(Stmt::Print("64".to_string())));
    }

    #[test]
    fn it_errors() {
        let tokens = Scanner::new("print".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(res, Some(Stmt::Error { line: 0, message: "Unfinished print statement".to_string() }));
    }

    #[test]
    fn it_doesnt_panick_unfinished() {
        let tokens = Scanner::new("print(".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(res, Some(Stmt::Error { line: 0, message: "Unfinished print statement".to_string() }));
    }

    #[test]
    fn it_works_partial_stmts() {
        let tokens = Scanner::new("var a;".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(res, Some(Stmt::VariableDef { ident: "a".to_string(), expr: None }));

        let tokens = Scanner::new("var  a;".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(res, Some(Stmt::VariableDef { ident: "a".to_string(), expr: None }));
    }

    #[test]
    fn it_works_stmts() {
        let tokens = Scanner::new("var a = \"foo\";".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(res, Some(Stmt::VariableDef { ident: "a".to_string(), expr: Some(Expr::Literal(Value::STRING("foo".to_string()))) }));

        let tokens = Scanner::new("var a  =  \"foo\";".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(res, Some(Stmt::VariableDef { ident: "a".to_string(), expr: Some(Expr::Literal(Value::STRING("foo".to_string()))) }));

        let tokens = Scanner::new("var a  = 2*8;".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(Stmt::VariableDef {
                ident: "a".to_string(),
                expr: Some(Expr::Binary {
                    left: Box::new(Expr::Literal(Value::NUMBER(2.0))),
                    operator: LexemeKind::Star,
                    right: Box::new(Expr::Literal(Value::NUMBER(8.0))),
                })
            })
        );
    }
}
