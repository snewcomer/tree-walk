pub(crate) mod expression;

use crate::lexer::{LexemeKind, Token};
pub use expression::{Expr, Value};

pub(crate) struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
}

pub(crate) fn debug_tree<T>(ast: Expr<T>) -> String {
    let mut st = String::new();
    st.push_str("(");
    if let Expr::Binary {
        left,
        operator,
        right,
    } = ast
    {
        let op = operator.to_string();
        st.push_str(&op);
        st.push_str(" ");

        let l = &(*left).debug();
        st.push_str(l);
        st.push_str(" ");

        let r = &(*right).debug();
        st.push_str(r);
    } else {
        println!("Not an expression");
    }

    st.push_str(")");
    st
}

impl Parser {
    pub(crate) fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, cursor: 0 }
    }

    // TODO: Do we need to return Options up the chain?  Did this to take advantage of ! error
    // handling but not exactly sure of my strategy
    pub(crate) fn parse<T>(&mut self) -> Option<Expr<T>>
    where
        T: std::fmt::Display
    {
        self.expression()
    }

    fn at_end(&self) -> bool {
        self.peek_kind() == Some(LexemeKind::EOF)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    fn peek_kind(&self) -> Option<LexemeKind> {
        self.peek()
            .and_then(|Token { lexeme, .. }| Some(lexeme.clone()))
    }

    fn expect(&mut self, kind: LexemeKind) {
        if self.at(kind) {
            self.cursor += 1;
        } else {
            todo!();
        }
    }

    fn at(&self, kind: LexemeKind) -> bool {
        if self.at_end() {
            return false;
        };
        self.peek_kind() == Some(kind)
    }

    fn error<T>(&self) -> Option<Expr<T>>
    where
        T: std::fmt::Display
    {
        Some(Expr::Error)
    }

    fn is_equal(&self, kinds: Vec<LexemeKind>) -> bool {
        let res = kinds.iter().find(|&k| self.at(k.clone()));
        res.is_some()
    }

    fn expression<T>(&mut self) -> Option<Expr<T>>
    where
        T: std::fmt::Display
    {
        self.equality()
    }

    fn equality<T>(&mut self) -> Option<Expr<T>>
    where
        T: std::fmt::Display
    {
        let mut expr = self.comparison();

        while self.is_equal(vec![LexemeKind::BangEqual, LexemeKind::EqualEqual]) {
            let operator = self.peek_kind().unwrap();
            self.cursor += 1;
            let right = self.comparison();
            expr = Some(Expr::Binary {
                left: Box::new(expr.unwrap()),
                operator,
                right: Box::new(right.unwrap()),
            })
        }

        expr
    }

    fn comparison<T>(&mut self) -> Option<Expr<T>>
    where
        T: std::fmt::Display
    {
        let mut expr = self.term();

        while self.is_equal(vec![
            LexemeKind::Greater,
            LexemeKind::GreaterEqual,
            LexemeKind::Less,
            LexemeKind::LessEqual,
        ]) {
            let operator = self.peek_kind().unwrap();

            self.cursor += 1;

            let right = self.term();
            expr = Some(Expr::Binary {
                left: Box::new(expr.unwrap()),
                operator,
                right: Box::new(right.unwrap()),
            })
        }

        expr
    }

    fn term<T>(&mut self) -> Option<Expr<T>>
    where
        T: std::fmt::Display
    {
        let mut expr = self.factor();

        while self.is_equal(vec![LexemeKind::Minus, LexemeKind::Plus]) {
            let operator = self.peek_kind().unwrap();

            self.cursor += 1;

            let right = self.factor();
            expr = Some(Expr::Binary {
                left: Box::new(expr.unwrap()), // 1
                operator, // +
                right: Box::new(right.unwrap()), // 1
            })
        }

        expr
    }

    fn factor<T>(&mut self) -> Option<Expr<T>>
    where
        T: std::fmt::Display
    {
        let mut expr = self.unary();

        while self.is_equal(vec![LexemeKind::Slash, LexemeKind::Star]) {
            let operator = self.peek_kind().unwrap();
            self.cursor += 1;
            let right = self.unary();
            expr = Some(Expr::Binary {
                left: Box::new(expr.unwrap()),
                operator,
                right: Box::new(right.unwrap()),
            })
        }

        expr
    }

    fn unary<T>(&mut self) -> Option<Expr<T>>
    where
        T: std::fmt::Display
    {
        let mut res = None;
        while self.is_equal(vec![LexemeKind::Bang, LexemeKind::Minus]) {
            let operator = self.peek_kind().unwrap();
            self.cursor += 1;
            let right = self.unary();
            res = Some(Expr::Unary {
                operator,
                right: Box::new(right.unwrap()),
            })
        }

        if res.is_some() {
            res
        } else {
            self.primary()
        }
    }

    // TODO: need to scope generic T to Value
    fn primary<T>(&mut self) -> Option<Expr<T>>
    where
        T: std::fmt::Display
    {
        let token = self.tokens.get(self.cursor).unwrap();
        match &token.lexeme {
            LexemeKind::FALSE => {
                self.cursor += 1;
                Some(Expr::Literal(Value(false)))
            }
            LexemeKind::TRUE => {
                self.cursor += 1;
                Some(Expr::Literal(Value(true)))
            }
            LexemeKind::STRING(st) => {
                self.cursor += 1;
                Some(Expr::Literal(Value(st.to_string())))
            }
            LexemeKind::NUMBER(num) => {
                self.cursor += 1;
                Some(Expr::Literal(Value(*num)))
            }
            LexemeKind::LeftParen => {
                self.cursor += 1;
                let expr = self.expression();

                self.expect(LexemeKind::RightParen);

                Some(Expr::Grouping(
                    Box::new(expr.unwrap()),
                ))
            }
            _ => self.error(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Scanner;

    #[test]
    fn it_handles_binary() {
        let tokens = Scanner::new("1+1".to_owned()).collect();
        let ast = Parser::new(tokens).parse().unwrap();
        assert_eq!(
            ast,
            Expr::Binary {
                left: Box::new(Expr::NUMBER(1.0)),
                operator: LexemeKind::Plus,
                right: Box::new(Expr::NUMBER(1.0)),
            }
        );

        let tokens = Scanner::new("1 == 1".to_owned()).collect();
        let ast = Parser::new(tokens).parse().unwrap();
        assert_eq!(
            ast,
            Expr::Binary {
                left: Box::new(Expr::NUMBER(1.0)),
                operator: LexemeKind::EqualEqual,
                right: Box::new(Expr::NUMBER(1.0)),
            }
        );
    }

    #[test]
    fn it_handles_comparison() {
        let tokens = Scanner::new("1 >= 2".to_owned()).collect();
        let ast = Parser::new(tokens).parse().unwrap();
        assert_eq!(
            ast,
            Expr::Binary {
                left: Box::new(Expr::NUMBER(1.0)),
                operator: LexemeKind::GreaterEqual,
                right: Box::new(Expr::NUMBER(2.0)),
            }
        );

        let tokens = Scanner::new("1 <= 2".to_owned()).collect();
        let ast = Parser::new(tokens).parse().unwrap();
        assert_eq!(
            ast,
            Expr::Binary {
                left: Box::new(Expr::NUMBER(1.0)),
                operator: LexemeKind::LessEqual,
                right: Box::new(Expr::NUMBER(2.0)),
            }
        );
    }

    #[test]
    fn it_handles_unary() {
        let tokens = Scanner::new("-1".to_owned()).collect();
        let ast = Parser::new(tokens).parse().unwrap();
        assert_eq!(
            ast,
            Expr::Unary {
                operator: LexemeKind::Minus,
                right: Box::new(Expr::NUMBER(1.0)),
            }
        );

        // let tokens = Scanner::new("+1".to_owned()).collect();
        // let ast = Parser::new(tokens).parse().unwrap();
        // assert_eq!(
        //     ast,
        //     Expr::Unary {
        //         operator: LexemeKind::Minus,
        //         right: Box::new(Expr::NUMBER(1.0)),
        //     }
        // );
    }

    #[test]
    fn it_errors_keyword() {
        let tokens = Scanner::new("and".to_owned()).collect();
        let ast = Parser::new(tokens).parse().unwrap();
        assert_eq!(
            ast,
            Expr::Error
        );
    }

    #[test]
    fn not_expression() {
        let tokens = Scanner::new("a".to_owned()).collect();
        let ast = Parser::new(tokens).parse().unwrap();
        assert_eq!(
            ast,
            Expr::Error
        );
    }

    #[test]
    fn it_works_parenthesized_expression() {
        let tokens = Scanner::new("(1+1)".to_owned()).collect();
        let ast = Parser::new(tokens).parse().unwrap();
        assert_eq!(
            ast,
            Expr::Grouping(
                Box::new(Expr::Binary {
                    left: Box::new(Expr::NUMBER(1.0)),
                    operator: LexemeKind::Plus,
                    right: Box::new(Expr::NUMBER(1.0)),
                }),
            )
        );
    }

    #[test]
    fn it_errors() {
        let tokens = Scanner::new("+1+1".to_owned()).collect();
        let ast = Parser::new(tokens).parse().unwrap();
        assert_eq!(
            ast,
            Expr::Binary {
                left: Box::new(Expr::Binary {
                    left: Box::new(Expr::Error),
                    operator: LexemeKind::Plus,
                    right: Box::new(Expr::NUMBER(1.0))
                }),
                operator: LexemeKind::Plus,
                right: Box::new(Expr::NUMBER(1.0)),
            }
        );
    }
}
