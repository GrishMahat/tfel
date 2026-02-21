use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::evaluator::Value;

#[derive(Debug, Clone)]
pub struct Environment {
    inner: Rc<RefCell<EnvironmentData>>,
}

#[derive(Debug)]
struct EnvironmentData {
    values: HashMap<String, Value>,
    parent: Option<Environment>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(EnvironmentData {
                values: HashMap::new(),
                parent: None,
            })),
        }
    }

    #[allow(dead_code)]
    pub fn new_enclosed(parent: Environment) -> Self {
        Self {
            inner: Rc::new(RefCell::new(EnvironmentData {
                values: HashMap::new(),
                parent: Some(parent),
            })),
        }
    }

    pub fn define(&self, name: impl Into<String>, value: Value) {
        // Same concept as normal languages: bind a variable in current scope.
        self.inner.borrow_mut().values.insert(name.into(), value);
    }

    pub fn assign(&self, name: &str, value: &Value) -> bool {
        let parent = {
            let mut borrowed = self.inner.borrow_mut();
            if borrowed.values.contains_key(name) {
                borrowed.values.insert(name.to_string(), value.clone());
                return true;
            }
            borrowed.parent.clone()
        };

        parent.is_some_and(|scope| scope.assign(name, value))
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        // Scope lookup order is normal and boring on purpose: local first, then parent.
        let parent = {
            let borrowed = self.inner.borrow();
            if let Some(value) = borrowed.values.get(name) {
                return Some(value.clone());
            }
            borrowed.parent.clone()
        };

        parent.and_then(|scope| scope.get(name))
    }

    pub fn snapshot_current_scope(&self) -> HashMap<String, Value> {
        self.inner.borrow().values.clone()
    }

    pub fn visible_names(&self) -> Vec<String> {
        let mut seen = HashSet::new();
        let mut names = Vec::new();
        self.collect_visible_names(&mut seen, &mut names);
        names.sort();
        names
    }

    fn collect_visible_names(&self, seen: &mut HashSet<String>, names: &mut Vec<String>) {
        let parent = {
            let borrowed = self.inner.borrow();
            for name in borrowed.values.keys() {
                if seen.insert(name.clone()) {
                    names.push(name.clone());
                }
            }
            borrowed.parent.clone()
        };

        if let Some(parent) = parent {
            parent.collect_visible_names(seen, names);
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}
