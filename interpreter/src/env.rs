use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub(crate) struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct UndefinedVariable;

impl Environment {
    pub(crate) fn new() -> Self {
        Environment {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub(crate) fn with(enclosing: Rc<RefCell<Environment>>) -> Self {
        Environment {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    pub(crate) fn define(&mut self, key: &str, value: Value) {
        self.values.insert(String::from(key), value);
    }

    pub(crate) fn get(&self, key: &str) -> Option<Value> {
        if let Some(val) = self.values.get(key) {
            Some(val.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.as_ref().borrow().get(key)
        } else {
            None
        }
    }

    #[allow(dead_code)]
    pub(crate) fn get_at(&self, dist: usize, key: &str) -> Option<Value> {
        if dist == 0 {
            self.get(key)
        } else {
            self.enclosing
                .as_ref()
                .and_then(|parent| parent.borrow().get_at(dist - 1, key))
        }
    }

    pub(crate) fn assign(&mut self, key: &str, value: Value) -> Result<(), UndefinedVariable> {
        if let Some(val) = self.values.get_mut(key) {
            *val = value;
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.as_ref().borrow_mut().assign(key, value)
        } else {
            Err(UndefinedVariable)
        }
    }

    #[allow(dead_code)]
    pub(crate) fn assign_at(
        &mut self,
        dist: usize,
        key: &str,
        value: Value,
    ) -> Result<(), UndefinedVariable> {
        if dist == 0 {
            if let Some(val) = self.values.get_mut(key) {
                *val = value;
                Ok(())
            } else {
                Err(UndefinedVariable)
            }
        } else if let Some(parent) = &self.enclosing {
            parent.as_ref().borrow_mut().assign_at(dist - 1, key, value)
        } else {
            Err(UndefinedVariable)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::env::{Environment, UndefinedVariable};
    use crate::value::Value;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn test_define_and_get() {
        let mut env = Environment::new();
        env.define("foo", Value::from("bar"));
        env.define("baz", Value::from(false));

        assert_eq!(env.get("foo"), Some(Value::from("bar")));
        assert_eq!(env.get("baz"), Some(Value::from(false)));
    }

    #[test]
    fn test_throw_error_if_undefined() {
        let mut env = Environment::new();
        assert_eq!(
            Err(UndefinedVariable),
            env.assign("foo", Value::from("bar"))
        );
        assert_eq!(None, env.get("foo"));
    }

    #[test]
    fn test_multi_level() {
        let env1 = Rc::new(RefCell::new(Environment::new()));
        env1.borrow_mut().define("foo", Value::from("bar"));

        {
            let mut env2 = Environment::with(env1.clone());
            env2.define("foo", Value::from("foofoo"));
            assert_eq!(env2.get_at(0, "foo"), Some(Value::from("foofoo")));
            assert_eq!(env2.get_at(1, "foo"), Some(Value::from("bar")));
            env2.assign_at(1, "foo", Value::from(false)).unwrap();
        }

        assert_eq!(env1.borrow().get("foo"), Some(Value::from(false)));
    }
}
