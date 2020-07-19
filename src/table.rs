use std::collections::HashMap;
use crate::*;

pub(crate) struct VarTable<'key, 'value> {
    values: HashMap<&'key str, RantValue<'value>>
}

impl VarTable<'_, '_>{
    /// Get a shallow clone of the value with the specified name.
    /// If the name doesn't exist, the method returns `None`.
    pub fn get(&self, name: &str) -> Option<RantValue> {
        self.values.get(name).map(|val| val.clone())
    }
}