//! # Declarative Records
//!
//! Each declarative Environment Record is associated with an ECMAScript program scope containing variable,
//! `constant`, `let`, `class`, `module`, `import`, and/or function declarations.
//! A declarative Environment Record binds the set of identifiers defined by the declarations contained within its scope.
//! More info:  [ECMA-262 sec-declarative-environment-records](https://tc39.es/ecma262/#sec-declarative-environment-records)

use crate::{
    builtins::value::{Value, ValueData},
    environment::{
        environment_record_trait::EnvironmentRecordTrait,
        lexical_environment::{Environment, EnvironmentType},
    },
    InternerSym,
};
use gc::Gc;
use gc_derive::{Finalize, Trace};
use std::collections::hash_map::HashMap;
use string_interner::Symbol;

/// Declarative Bindings have a few properties for book keeping purposes, such as mutability (const vs let).
/// Can it be deleted? and strict mode.
///
/// So we need to create a struct to hold these values.
/// From this point onwards, a binding is referring to one of these structures.
#[derive(Trace, Finalize, Debug, Clone)]
pub struct DeclarativeEnvironmentRecordBinding {
    pub value: Option<Value>,
    pub can_delete: bool,
    pub mutable: bool,
    pub strict: bool,
}

/// A declarative Environment Record binds the set of identifiers defined by the
/// declarations contained within its scope.
#[derive(Debug, Trace, Finalize, Clone)]
pub struct DeclarativeEnvironmentRecord {
    pub env_rec: HashMap<InternerSym, DeclarativeEnvironmentRecordBinding>,
    pub outer_env: Option<Environment>,
}

impl EnvironmentRecordTrait for DeclarativeEnvironmentRecord {
    fn has_binding(&self, name: InternerSym) -> bool {
        self.env_rec.contains_key(&name)
    }

    fn create_mutable_binding(&mut self, name: InternerSym, deletion: bool) {
        if self.env_rec.contains_key(&name) {
            // TODO: change this when error handling comes into play
            panic!(
                "Identifier str#{:#X} has already been declared",
                name.to_usize()
            );
        }

        self.env_rec.insert(
            name,
            DeclarativeEnvironmentRecordBinding {
                value: None,
                can_delete: deletion,
                mutable: true,
                strict: false,
            },
        );
    }

    fn create_immutable_binding(&mut self, name: InternerSym, strict: bool) -> bool {
        if self.env_rec.contains_key(&name) {
            // TODO: change this when error handling comes into play
            panic!(
                "Identifier str#{:#X} has already been declared",
                name.to_usize()
            );
        }

        self.env_rec.insert(
            name,
            DeclarativeEnvironmentRecordBinding {
                value: None,
                can_delete: true,
                mutable: false,
                strict,
            },
        );

        true
    }

    fn initialize_binding(&mut self, name: InternerSym, value: Value) {
        if let Some(ref mut record) = self.env_rec.get_mut(&name) {
            if record.value.is_none() {
                record.value = Some(value);
            } else {
                // TODO: change this when error handling comes into play
                panic!(
                    "Identifier str#{:#X} has already been defined",
                    name.to_usize()
                );
            }
        }
    }

    #[allow(clippy::else_if_without_else)]
    fn set_mutable_binding(&mut self, name: InternerSym, value: Value, mut strict: bool) {
        if self.env_rec.get(&name).is_none() {
            if strict {
                // TODO: change this when error handling comes into play
                panic!(
                    "Reference Error: Cannot set mutable binding for str#{:#X}",
                    name.to_usize()
                );
            }

            self.create_mutable_binding(name, true);
            self.initialize_binding(name, value);
            return;
        }

        let record: &mut DeclarativeEnvironmentRecordBinding = self.env_rec.get_mut(&name).unwrap();
        if record.strict {
            strict = true
        }
        if record.value.is_none() {
            // TODO: change this when error handling comes into play
            panic!(
                "Reference Error: Cannot set mutable binding for str#{:#X}",
                name.to_usize()
            );
        }

        if record.mutable {
            record.value = Some(value);
        } else if strict {
            // TODO: change this when error handling comes into play
            panic!(
                "TypeError: Cannot mutate an immutable binding str#{:#X}",
                name.to_usize()
            );
        }
    }

    fn get_binding_value(&self, name: InternerSym, _strict: bool) -> Value {
        if let Some(binding) = self.env_rec.get(&name) {
            binding
                .value
                .as_ref()
                .expect("Could not get record as reference")
                .clone()
        } else {
            // TODO: change this when error handling comes into play
            panic!(
                "ReferenceError: Cannot get binding value for {}",
                name.to_usize()
            );
        }
    }

    fn delete_binding(&mut self, name: InternerSym) -> bool {
        match self.env_rec.get(&name) {
            Some(binding) => {
                if binding.can_delete {
                    self.env_rec.remove(&name);
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn has_this_binding(&self) -> bool {
        false
    }

    fn has_super_binding(&self) -> bool {
        false
    }

    fn with_base_object(&self) -> Value {
        Gc::new(ValueData::Undefined)
    }

    fn get_outer_environment(&self) -> Option<Environment> {
        self.outer_env.as_ref().cloned()
    }

    fn set_outer_environment(&mut self, env: Environment) {
        self.outer_env = Some(env);
    }

    fn get_environment_type(&self) -> EnvironmentType {
        EnvironmentType::Declarative
    }

    fn get_global_object(&self) -> Option<Value> {
        match &self.outer_env {
            Some(outer) => outer.borrow().get_global_object(),
            None => None,
        }
    }
}
