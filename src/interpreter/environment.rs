use std::cell::RefCell;
use std::collections;
use std::rc::Rc;
use std::collections::HashMap;
use crate::parser::Value;
use super::RuntimeError;

pub struct Environment {
    pub variables: collections::HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>, // pattern especially useful when a function will cannot borrow a field as mutable. Once something already has a reference, you can't then borrow as mutable
}

impl Environment {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: Value) -> Result<(), RuntimeError> {
        if !self.variables.contains_key(&name) {
            if let Some(ref encl) = self.enclosing {
                // Rc<RefCell> - pointer with shared ownership with interior mutability
                // need a ref b/c enclosing value does not implement the Copy trait
                return encl.borrow_mut().assign(name.clone(), value.clone());
            } else {
                // if can never find, then error
                return Err(RuntimeError {
                    line: 0,
                    message: format!("Variable {} does not exist", name),
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
                Err(RuntimeError {
                    line: 0,
                    message: format!("Variable {} does not exist", name),
                })
            }
        }
    }
}
