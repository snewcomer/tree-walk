use crate::lexer::LexemeKind;

#[derive(Debug, PartialEq)]
pub(crate) enum Expr {
    Binary {
        left: Box<Expr>,
        operator: LexemeKind,
        right: Box<Expr>,
    },
    BOOLEAN(bool),
    STRING(String),
    NUMBER(f64),
    Unary {
        operator: LexemeKind,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Error,
}

impl Expr {
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
            Expr::BOOLEAN(true) => "true".to_string(),
            Expr::BOOLEAN(false) => "true".to_string(),
            Expr::STRING(st) => st.to_string(),
            Expr::NUMBER(n) => n.to_string(),
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

