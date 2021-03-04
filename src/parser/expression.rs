use std::fmt;
use std::ops::Deref;
use crate::lexer::LexemeKind;
use crate::visitor::{Visitor, RuntimeError};

#[derive(Debug, PartialEq)]
pub enum Expr<T> {
    Binary {
        left: Box<Expr<T>>,
        operator: LexemeKind,
        right: Box<Expr<T>>,
    },
    Literal(Value<T>),
    Unary {
        operator: LexemeKind,
        right: Box<Expr<T>>,
    },
    Grouping(Box<Expr<T>>),
    Error,
}

#[derive(Debug, PartialEq)]
pub struct Value<T>(pub T);

impl<T> fmt::Display for Value<T>
where T: fmt::Display + fmt::Debug
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl<T> Deref for Value<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> Expr<T> {
    pub(crate) fn accept(&self, visitor: &mut dyn Visitor<T>) -> Result<T, RuntimeError> {
        match *self {
            Expr::Binary { operator, left, right } => {
                visitor.visit_binary(*left, operator, *right)
            }
            Expr::Unary { operator, right } => {
                visitor.visit_unary(operator, *right)
            }
            Expr::Grouping(val) => {
                visitor.visit_grouping(*val)
            }
            Expr::Literal(v) => {
                visitor.visit_literal(v)
            }
            Expr::Error => todo!()
        }
    }

    pub(crate) fn debug(&self) -> String {
        match &self {
            Expr::Binary { operator, left, right } => {
                let mut st = String::new();
                st.push_str("(");

                let op = operator.to_string();
                st.push_str(&op);
                st.push_str(" ");

                let l = &(*left).debug();
                st.push_str(l);
                st.push_str(" ");

                let r = &(*right).debug();
                st.push_str(r);

                st
            },
            Expr::Literal(v) => {
                (*v).to_string()
            }
            Expr::Unary { operator, right } => {
                let mut st = String::new();
                st.push_str("( ");

                let op = operator.to_string();
                st.push_str(&op);
                st.push_str(" ");

                let r = &(*right).debug();
                st.push_str(r);
                st.push_str(" ");

                st
            },
            Expr::Grouping(value) => {
                value.debug()
            },
            Expr::Error => "~ERROR~".to_string(),
        }
    }
}

