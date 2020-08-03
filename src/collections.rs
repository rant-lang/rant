use std::{rc::Rc, collections::HashMap};
use crate::RantValue;

#[derive(Debug)]
pub struct RantMap {
    map: HashMap<String, RantValue>,
    prototype: Option<Rc<RantMap>>
}

impl RantMap {
    pub fn new() -> Self {
        Self {
            map: Default::default(),
            prototype: None
        }
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl Default for RantMap {
    fn default() -> Self {
        RantMap::new()
    }
}