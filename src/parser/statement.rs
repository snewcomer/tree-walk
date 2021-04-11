use crate::lexer::{LexemeKind, Token};
use super::expression::Expr;
use super::{Parser, Value};
use crate::visitor::StatementVisitor;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Block(Box<Vec<Stmt>>),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Box<Option<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Func {
        ident: String,
        parameters: Vec<Token>,
        body: Box<Stmt>,
    },
    VariableDef {
        ident: String,
        expr: Option<Expr>,
    },
    Print(Option<Expr>),
    Expr(Expr),
    Error {
        line: usize,
        message: String,
    }
}

impl Stmt {
    pub(crate) fn accept<T>(&self, visitor: &mut dyn StatementVisitor<T>) -> T {
        match self {
            Stmt::Block(stmts) => {
                visitor.visit_block(stmts)
            }
            Stmt::If { condition, then_branch, else_branch } => {
                visitor.visit_if(condition, then_branch, else_branch)
            }
            Stmt::While { condition, body } => {
                visitor.visit_while(condition, body)
            }
            Stmt::Func { ident, parameters, body } => {
                visitor.visit_func_declaration(ident, parameters, body)
            }
            Stmt::VariableDef { ident, expr } => {
                visitor.visit_variable_def(ident, expr)
            }
            Stmt::Print(expr) => {
                visitor.visit_print(expr)
            }
            Stmt::Expr(expr) => {
                visitor.visit_expr(expr)
            }
            Stmt::Error { line, message } => {
                visitor.visit_error(line, message)
            }
        }
    }
}

pub(crate) fn parse(p: &mut Parser) -> Option<Stmt> {
    p.eat_whitespace();

    if p.at(LexemeKind::VAR) {
        p.cursor += 1;
        // ultimately, this is what our program is made up of
        declaration_stmt(p)
    } else if p.at(LexemeKind::FUN) {
        p.cursor += 1;
        func_declaration(p)
    } else if p.at(LexemeKind::FOR) {
        p.cursor += 1;
        for_statement(p)
    } else if p.at(LexemeKind::IF) {
        p.cursor += 1;
        if_statement(p)
    } else if p.at(LexemeKind::WHILE) {
        p.cursor += 1;
        while_statement(p)
    } else if p.at(LexemeKind::LeftBrace) {
        p.cursor += 1;

        block(p)
    } else if p.at(LexemeKind::Semicolon) {
        p.cursor += 1;

        None
    } else {
        statement(p)
    }
}

fn func_declaration(p: &mut Parser) -> Option<Stmt> {
    p.eat_whitespace();

    let mut ident = "unknown".to_string();
    if let Some(LexemeKind::IDENTIFIER(name)) = p.peek_kind() {
        ident = name;
    }

    // consume identifier
    p.cursor += 1;
    p.eat_whitespace();

    let _ = p.expect(LexemeKind::LeftParen);

    let mut parameters = vec![];
    while p.peek_kind() != Some(LexemeKind::RightParen) {
        p.eat_whitespace();

        if let Some(LexemeKind::IDENTIFIER(_)) = p.peek_kind() {
            // we should clone this
            parameters.push(p.peek().unwrap().clone());

            p.cursor += 1;
            p.eat_whitespace();
        }

        if let Some(LexemeKind::Comma) = p.peek_kind() {
            // consume comma
            p.cursor += 1;
            p.eat_whitespace();
        }

        p.eat_whitespace();
    }

    let _ = p.expect(LexemeKind::RightParen);

    p.eat_whitespace();

    let _ = p.expect(LexemeKind::LeftBrace);

    let block = block(p).unwrap();

    Some(Stmt::Func { ident, parameters, body: Box::new(block) })
}

fn if_statement(p: &mut Parser) -> Option<Stmt> {
    p.eat_whitespace();

    let _ = p.expect(LexemeKind::LeftParen);
    p.eat_whitespace();
    let condition = p.expression()?;
    p.eat_whitespace();
    let _ = p.expect(LexemeKind::RightParen);

    let then_branch = parse(p).unwrap();
    p.eat_whitespace();

    let mut else_branch = None;
    if p.at(LexemeKind::ELSE) {
        p.cursor += 1;
        p.eat_whitespace();
        else_branch = parse(p);
    }

    Some(Stmt::If { condition, then_branch: Box::new(then_branch), else_branch: Box::new(else_branch) })
}

fn for_statement(p: &mut Parser) -> Option<Stmt> {
    p.eat_whitespace();

    let _ = p.expect(LexemeKind::LeftParen);

    p.eat_whitespace();

    // for (var a = 0; ....)
    let initializer = match p.peek_kind() {
        Some(LexemeKind::Semicolon) => {
            p.cursor += 1;
            None
        }
        Some(LexemeKind::VAR) => {
            p.cursor += 1;
            declaration_stmt(p)
        }
        _ => {
            p.cursor += 1;
            Some(Stmt::Expr(p.expression().unwrap()))
        }
    };

    p.eat_whitespace();

    // for (...; a < 5 ;....)
    let mut condition = match p.peek_kind() {
        Some(LexemeKind::Semicolon) => None,
        _ => p.expression()
    };

    p.cursor += 1;
    p.eat_whitespace();

    let increment = match p.peek_kind() {
        Some(LexemeKind::RightParen) => None,
        _ => p.expression()
    };

    let _ = p.expect(LexemeKind::RightParen);

    p.eat_whitespace();

    // eat LEFT_BRACE {
    p.cursor += 1;
    let mut body = block(p);

    if increment.is_some() {
        body = Some(Stmt::Block(Box::new(vec![body.unwrap(), Stmt::Expr(increment.unwrap())])));
    }

    if condition.is_none() {
        condition = Some(Expr::Literal(Value::BOOLEAN(true)));
    }

    body = Some(Stmt::While { condition: condition.unwrap(), body: Box::new(body.unwrap()) });

    if initializer.is_some() {
        body = Some(Stmt::Block(Box::new(vec![initializer.unwrap(), body.unwrap()])));
    }

    body
}

fn while_statement(p: &mut Parser) -> Option<Stmt> {
    p.eat_whitespace();

    let _ = p.expect(LexemeKind::LeftParen);
    p.eat_whitespace();
    let condition = p.expression()?;
    p.eat_whitespace();
    let _ = p.expect(LexemeKind::RightParen);

    let body = parse(p);

    Some(Stmt::While { condition, body: Box::new(body.unwrap()) })
}

fn block(p: &mut Parser) -> Option<Stmt> {
    let mut v: Vec<Stmt> = vec![];

    p.eat_whitespace();

    while p.at(LexemeKind::RightBrace) == false {
        if let Some(res) = parse(p) {
            v.push(res);
        }

        p.eat_whitespace();
    }

    p.eat_whitespace();

    p.cursor += 1; // RightBrace

    Some(Stmt::Block(Box::new(v)))
}

pub(crate) fn statement(p: &mut Parser) -> Option<Stmt> {
    if p.at(LexemeKind::PRINT) {
        p.cursor += 1; // PRINT
        print_stmt(p)
    } else {
        // fallthrough to expression
        let expr = p.expression()?;
        Some(Stmt::Expr(expr))
    }
}

fn declaration_stmt(p: &mut Parser) -> Option<Stmt> {
    // var x = 1+1;
    p.eat_whitespace();

    match p.expression() {
        Some(Expr::Assign { name, expr }) => {
            let stmt = Some(Stmt::VariableDef { ident: name, expr: Some(*expr) });
            // assert!(p.at(LexemeKind::Semicolon));
            p.cursor += 1;
            stmt
        }
        Some(Expr::Variable(name)) => {
            let stmt = Some(Stmt::VariableDef { ident: name, expr: None });
            // assert!(p.at(LexemeKind::Semicolon));
            p.cursor += 1;
            stmt
        }
        _ => Some(Stmt::Error { line: 0, message: "Unfinished right hand assignment".to_string() })
    }
}

fn print_stmt(p: &mut Parser) -> Option<Stmt> {
    p.cursor += 1; // LeftParen

    match p.peek_kind() {
        Some(LexemeKind::RightParen) => {
            p.cursor += 1; // RightParen
            // print();
            Some(Stmt::Print(None))
        }
        _ => {
            let expr = p.expression();

            if let Ok(()) = p.expect(LexemeKind::RightParen) {
                p.cursor += 1; // RightParen

                // semicolon optional
                if let Ok(_) = p.expect(LexemeKind::Semicolon) {
                   p.cursor += 1;
                }

                Some(Stmt::Print(expr))
            } else {
                Some(Stmt::Error { line: 0, message: "Unfinished print statement".to_string() })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Scanner, Token};
    use crate::parser::{Parser, Value};

    #[test]
    fn it_stmt_works() {
        let tokens = Scanner::new("print(1)".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(Stmt::Print(Some(Expr::Literal(Value::NUMBER(1.0)))))
        );
    }

    #[test]
    fn it_stmt_works_strings() {
        let tokens = Scanner::new("print(\"foo\")".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(Stmt::Print(Some(Expr::Literal(Value::STRING("foo".to_string())))))
        );
    }

    #[test]
    fn it_accepts_nothing() {
        let tokens = Scanner::new("print()".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(Stmt::Print(None))
        );
    }

    #[test]
    fn it_accepts_expressions() {
        let tokens = Scanner::new("print(8*8)".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(Stmt::Print(Some(Expr::Binary {
                left: Box::new(Expr::Literal(Value::NUMBER(8.0))),
                operator: LexemeKind::Star,
                right: Box::new(Expr::Literal(Value::NUMBER(8.0))),
            })))
        );

        let tokens = Scanner::new("print(8 * 8)".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(Stmt::Print(Some(Expr::Binary {
                left: Box::new(Expr::Literal(Value::NUMBER(8.0))),
                operator: LexemeKind::Star,
                right: Box::new(Expr::Literal(Value::NUMBER(8.0))),
            })))
        );

        let tokens = Scanner::new("print(8 *  8)".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(Stmt::Print(Some(Expr::Binary {
                left: Box::new(Expr::Literal(Value::NUMBER(8.0))),
                operator: LexemeKind::Star,
                right: Box::new(Expr::Literal(Value::NUMBER(8.0))),
            })))
        );
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

    #[test]
    fn it_works_multiline() {
        let tokens = Scanner::new("var a = 2;
print(a);".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(res, Some(Stmt::VariableDef { ident: "a".to_string(), expr: Some(Expr::Literal(Value::NUMBER(2.0)))}));
    }

    #[test]
    fn it_errors_expression_l_value() {
        let tokens = Scanner::new("a + b = 2".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        // error in parser expr
        assert_eq!(res, Some(Stmt::Expr(Expr::Error { line: 0, message: "Invalid left hand assignment expression".to_string() })));
    }

    #[test]
    fn it_errors_stmt() {
        let tokens = Scanner::new("var a =".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(res, Some(Stmt::Error { line: 0, message: "Unfinished right hand assignment".to_string() }));
    }

    #[test]
    fn it_works_block_no_spaces() {
        let tokens = Scanner::new("{var a = 2; print(a);}".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(
                Stmt::Block(
                    Box::new(
                        vec![
                            Stmt::VariableDef { ident: "a".to_string(), expr: Some(Expr::Literal(Value::NUMBER(2.0))) },
                            Stmt::Print(Some(Expr::Variable("a".to_string()))),
                        ]
                    )
                )
            )
        );
    }

    #[test]
    fn simple_block() {
        let tokens = Scanner::new("{ 1; }".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(
                Stmt::Block(
                    Box::new(
                        vec![
                            Stmt::Expr(Expr::Literal(Value::NUMBER(1.0))),
                        ]
                    )
                )
            )
        );
    }

    #[test]
    fn it_works_block_spaces() {
        let tokens = Scanner::new("{
            var a = 2;
            print(a); }".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(
                Stmt::Block(
                    Box::new(
                        vec![
                            Stmt::VariableDef { ident: "a".to_string(), expr: Some(Expr::Literal(Value::NUMBER(2.0))) },
                            Stmt::Print(Some(Expr::Variable("a".to_string()))),
                        ]
                    )
                )
            )
        );
    }

    #[test]
    fn it_works_if_stmt() {
        let tokens = Scanner::new("if (true) {
            var a = 2;
            print(a);
        }".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(
                Stmt::If {
                    condition: Expr::Literal(Value::BOOLEAN(true)),
                    then_branch: Box::new(Stmt::Block(Box::new(vec![
                        Stmt::VariableDef { ident: "a".to_string(), expr: Some(Expr::Literal(Value::NUMBER(2.0))) },
                        Stmt::Print(Some(Expr::Variable("a".to_string()))),
                    ]))),
                    else_branch: Box::new(None),
                }
            )
        );
    }

    #[test]
    fn it_works_if_inline_stmt() {
        let tokens = Scanner::new("if (true) print(2);".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(
                Stmt::If {
                    condition: Expr::Literal(Value::BOOLEAN(true)),
                    then_branch: Box::new(Stmt::Print(Some(Expr::Literal(Value::NUMBER(2.0))))),
                    else_branch: Box::new(None),
                }
            )
        );
    }

    #[test]
    fn it_works_if_else_stmt() {
        let tokens = Scanner::new("
if (true) {
    var a = 2;
    print(a);
} else {
    var b = 3;
    print(b);
}".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(
                Stmt::If {
                    condition: Expr::Literal(Value::BOOLEAN(true)),
                    then_branch: Box::new(Stmt::Block(Box::new(vec![
                        Stmt::VariableDef { ident: "a".to_string(), expr: Some(Expr::Literal(Value::NUMBER(2.0))) },
                        Stmt::Print(Some(Expr::Variable("a".to_string()))),
                    ]))),
                    else_branch: Box::new(Some(Stmt::Block(Box::new(vec![
                        Stmt::VariableDef { ident: "b".to_string(), expr: Some(Expr::Literal(Value::NUMBER(3.0))) },
                        Stmt::Print(Some(Expr::Variable("b".to_string()))),
                    ])))),
                }
            )
        );
    }

    #[test]
    fn it_works_while_stmt() {
        let tokens = Scanner::new("
        while (true) {
            var a = 2;
            print(a);
        }".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(
                Stmt::While {
                    condition: Expr::Literal(Value::BOOLEAN(true)),
                    body: Box::new(Stmt::Block(Box::new(vec![
                        Stmt::VariableDef { ident: "a".to_string(), expr: Some(Expr::Literal(Value::NUMBER(2.0))) },
                        Stmt::Print(Some(Expr::Variable("a".to_string()))),
                    ]))),
                }
            )
        );
    }

    #[test]
    fn it_works_for_stmt() {
        let tokens = Scanner::new("
        for (var a = 0; a < 5; a = a + 1) {
            print(a);
        }".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(
                Stmt::Block(Box::new(vec![
                    Stmt::VariableDef { ident: "a".to_string(), expr: Some(Expr::Literal(Value::NUMBER(0.0))) },
                    Stmt::While {
                        condition: Expr::Binary {
                            left: Box::new(Expr::Variable("a".to_string())),
                            operator: LexemeKind::Less,
                            right: Box::new(Expr::Literal(Value::NUMBER(5.0)))
                        },
                        body: Box::new(Stmt::Block(Box::new(vec![
                            Stmt::Block(Box::new(vec![Stmt::Print(Some(Expr::Variable("a".to_string())))])),
                            Stmt::Expr(Expr::Assign {
                                name: "a".to_string(),
                                expr: Box::new(Expr::Binary {
                                    left: Box::new(Expr::Variable("a".to_string())),
                                    operator: LexemeKind::Plus,
                                    right: Box::new(Expr::Literal(Value::NUMBER(1.0)))
                                })
                            }),
                        ]))),
                    }
                ]))
            )
        )
    }

    #[test]
    fn empty_func_works() {
        let tokens = Scanner::new("fun foo(age) {}".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(Stmt::Func {
                ident: "foo".to_string(),
                parameters: vec![Token { line: 0, lexeme: LexemeKind::IDENTIFIER("age".to_string()) }],
                body: Box::new(Stmt::Block(Box::new(vec![])))
            })
        );
    }

    #[test]
    fn func_works() {
        let tokens = Scanner::new("fun foo(age) { print(1); }".to_owned()).collect();
        let mut p = Parser::new(tokens);
        let res = parse(&mut p);
        assert_eq!(
            res,
            Some(Stmt::Func {
                ident: "foo".to_string(),
                parameters: vec![Token { line: 0, lexeme: LexemeKind::IDENTIFIER("age".to_string()) }],
                body: Box::new(Stmt::Block(Box::new(vec![
                    Stmt::Print(Some(Expr::Literal(Value::NUMBER(1.0))))
                ])))
            })
        );
    }
}
