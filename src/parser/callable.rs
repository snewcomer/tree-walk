use super::super::interpreter::{Environment, Interpreter, InterpreterResult};
use super::expression::Value;
use super::statement::Stmt;
use crate::lexer::{LexemeKind, Token};

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
        closure: Environment, // inner most closure wins
    }
}

impl Callable {
    pub fn arity(self: &Self) -> usize {
        match self {
            Self::BuiltIn { arity, .. } => *arity,
            Self::UserFunction { params, .. } => params.len(),
        }
    }

    // this is called from the interpreter
    pub fn call(self: &Self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> InterpreterResult {
        match self {
            Self::BuiltIn { func, .. } => func(interpreter, arguments),
            Self::UserFunction { params, body, closure, .. } => {
                // here we have the arguments passed to the function call.  These are the eval'ed
                // results. We need to assign the arguments with the params and then evaluate the
                // body.  First we need to create a new block with the enclosing environment.
                // We dont want to mutate the existing environment however.
                let mut declaration_env = Environment::new_env_with_scope(&closure);
                for (p, a) in params.iter().zip(arguments.iter()) {
                    if let LexemeKind::IDENTIFIER(ref s) = p.lexeme {
                        declaration_env.define(s.to_owned(), a.clone());
                    }
                }

                let mut block_interpreter = Interpreter::new_with_scope(declaration_env);
                block_interpreter.execute(body)
            }
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
        Ok(())
    }
}
