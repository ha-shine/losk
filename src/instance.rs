use crate::callable::{Callable, Class};
use crate::token::Literal;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct Instance {
    class: Rc<Class>,
    fields: HashMap<String, Literal>,
}

impl Instance {
    pub(crate) fn new(class: Rc<Class>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Instance {
            class,
            fields: HashMap::new(),
        }))
    }

    pub(crate) fn get(&self, name: &str) -> Option<Literal> {
        self.fields.get(name).cloned()
    }

    pub(crate) fn set(&mut self, name: &str, value: Literal) -> Literal {
        self.fields.insert(String::from(name), value.clone());
        value
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Instance {}.class>", self.class.name())
    }
}
