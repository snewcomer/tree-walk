use std::cell::RefCell;
use std::collections;
use std::rc::Rc;
use std::collections::HashMap;
use std::time::SystemTime;
use crate::parser::{Callable, Value};
use super::RuntimeError;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    pub variables: collections::HashMap<String, Value>,
    pub enclosing: Option<Rc<RefCell<Environment>>>, // Rc pattern especially useful when a function will cannot borrow a field as mutable. Once something already has a reference, you can't then borrow as mutable
    // place to mutate and read from enclosing.  So shared ownership over the enclosing allows us to pass it around to various scopes. But b/c cloned, the original Environment does not inherit values after mutation
}

impl Environment {
    pub fn new() -> Self {
        let mut m = HashMap::new();

        // TODO: extract definining builtin functions
        let clock = Value::Callable(Callable::BuiltIn {
            arity: 0,
            func: |_, _| {
                Ok(Value::NUMBER(SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).expect("whoops").as_millis() as f64))
            }
        });

        m.insert("clock".to_string(), clock);

        Self {
            variables: m,
            enclosing: None,
        }
    }

    pub fn new_with_scope(env: &Rc<RefCell<Environment>>) -> Self {
        // create a new inner scope
        Self {
            variables: HashMap::new(), // empty b/c retrieve will look up enclosing chain for variables if need be
            enclosing: Some(env.clone()),
        }
    }

    pub fn new_env_with_scope(env: &Environment) -> Self {
        // create a new inner scope
        Self {
            variables: HashMap::new(), // empty b/c retrieve will look up enclosing chain for variables if need be
            enclosing: Some(Rc::new(RefCell::new(env.clone())))
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: Value) -> Result<(), RuntimeError> {
        if !self.variables.contains_key(&name) {
            // if inner most scope self.variables does not contain variable, check outer for variable
            if let Some(ref encl) = self.enclosing {
                // Rc<RefCell> - pointer with shared ownership with interior mutability
                // need a ref b/c enclosing value does not implement the Copy trait
                return encl.borrow_mut().assign(name.clone(), value.clone());
            } else {
                // if can never find, then error
                // for key in self.variables.keys() {
                //     eprintln!("{:?}", key);
                // }
                return Err(RuntimeError {
                    line: 0,
                    message: format!("Variable \"{}\" does not exist", name),
                });
            }
        }

        self.variables.insert(name, value);

        Ok(())
    }

    pub fn retrieve(&self, name: &str) -> Result<Value, RuntimeError> {
        let val = self.variables.get(name);
        if val.is_some() {
            Ok(val.unwrap().clone())
        } else {
            // check enclosing scope recursively. Variables are lexically scoped so we need to do this
            if let Some(ref enclosing) = self.enclosing {
                let enc = enclosing.borrow();
                let val = enc.retrieve(name);
                match val {
                    Ok(val) => Ok(val.clone()),
                    Err(RuntimeError { line, message }) => Err(RuntimeError { line, message })
                }
            } else {
                // if can never find, then error
                // for key in self.variables.keys() {
                //     eprintln!("{:?}", key);
                // }
                Err(RuntimeError {
                    line: 0,
                    message: format!("Variable \"{}\" does not exist", name),
                })
            }
        }
    }
}
