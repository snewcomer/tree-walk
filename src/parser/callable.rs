use super::super::interpreter::{Interpreter, InterpreterResult};
use super::expression::Value;
use super::statement::Stmt;
use crate::lexer::Token;

#[derive(Clone)]
pub enum Callable {
    BuiltIn {
        arity: usize,
        func: fn(interpreter: &mut Interpreter, Vec<Value>) ->  InterpreterResult,
    },
    UserFunction {
        name: String,
        params: Vec<Token>,
        body: Box<Stmt>,
    }
}

impl Callable {
    pub fn arity(self: &Self) -> usize {
        match self {
            Self::BuiltIn { arity, .. } => *arity,
            Self::UserFunction { params, .. } => params.len(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Self::BuiltIn { .. } => "<native fn>".to_owned(),
            Self::UserFunction { name, .. } => format!("<fn {}>", name),
        }
    }
}

impl PartialEq for Callable {
    fn eq(&self, other: &Callable) -> bool {
        return self == other;
    }
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        todo!()
    }
}
