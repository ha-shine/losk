use crate::token::Literal;
use std::collections::HashMap;

pub(crate) struct Environment<'a, 'b>
where
    'b: 'a,
{
    enclosing: Option<&'a mut Environment<'b, 'b>>,
    values: HashMap<String, Literal>,
}

impl<'a, 'b> Environment<'a, 'b>
where
    'b: 'a,
{
    pub(crate) fn new() -> Self {
        Environment {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub(crate) fn with(enclosing: &'a mut Environment<'b, 'b>) -> Self {
        Environment {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    pub(crate) fn get(&self, key: &str) -> Option<&Literal> {
        self.values.get(key)
    }

    pub(crate) fn get_at(&self, dist: usize, key: &str) -> Option<&Literal> {
        if dist == 0 {
            self.get(key)
        } else {
            self.enclosing
                .as_ref()
                .and_then(|parent| parent.get_at(dist - 1, key))
        }
    }

    pub(crate) fn assign(&mut self, key: String, value: Literal) {
        self.values.insert(key, value);
    }

    pub(crate) fn assign_at(&mut self, dist: usize, key: String, value: Literal) {
        if dist == 0 {
            self.assign(key, value);
        } else if let Some(parent) = self.enclosing.as_mut() {
            parent.assign_at(dist - 1, key, value)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::env::Environment;
    use crate::token::Literal;

    #[test]
    fn test_assign_and_get() {
        let mut env = Environment::new();
        env.assign(String::from("foo"), Literal::from("bar"));
        env.assign(String::from("baz"), Literal::from(false));

        assert_eq!(env.get("foo"), Some(&Literal::from("bar")));
        assert_eq!(env.get("baz"), Some(&Literal::from(false)));
    }

    #[test]
    fn test_multi_level() {
        let mut env1 = Environment::new();
        env1.assign(String::from("foo"), Literal::from("bar"));

        {
            let mut env2 = Environment::with(&mut env1);
            env2.assign(String::from("foo"), Literal::from("foofoo"));
            assert_eq!(env2.get_at(0, "foo"), Some(&Literal::from("foofoo")));
            assert_eq!(env2.get_at(1, "foo"), Some(&Literal::from("bar")));
            env2.assign_at(1, String::from("foo"), Literal::from(false));
        }

        assert_eq!(env1.get("foo"), Some(&Literal::from(false)));
    }
}
