use crate::{
    builtins::{
        object::{Object, ObjectInternalMethods, ObjectKind, PROTOTYPE},
        property::Property,
        value::{same_value, to_value, undefined, ResultValue, Value, ValueData},
    },
    environment::lexical_environment::Environment,
    realm::Realm,
    syntax::ast::node::{FormalParameter, Node},
    Interpreter,
};

use gc::{custom_trace, Gc};
use gc_derive::{Finalize, Trace};
use std::collections::HashMap;
use std::fmt::{self, Debug};

/// _fn(this, arguments, ctx) -> ResultValue_ - The signature of a built-in function
pub type NativeFunctionData = fn(&Value, &[Value], &mut Interpreter) -> ResultValue;
/// Sets the functionKind
#[derive(Trace, Finalize, Debug, Clone)]
pub enum FunctionKind {
    Normal,
    ClassConstructor,
    Generator,
    Async,
    AsyncGenerator,
    NonConstructor,
}
/// Sets the ConstructorKind
#[derive(Debug, Copy, Clone)]
pub enum ConstructorKind {
    Base,
    Derived,
}
/// Defines how this references are interpreted within the formal parameters and code body of the function.
#[derive(Debug, Copy, Clone)]
pub enum ThisMode {
    Lexical,
    Strict,
    Global,
}

/// FunctionBody is Boa specific, it will either be Rust code or JavaScript code (Block Node)
#[derive(Clone)]
pub enum FunctionBody {
    BuiltIn(NativeFunctionData),
    Ordinary(Node),
}

/// Boa representation of a Function Object.   
/// <https://tc39.es/ecma262/#sec-ecmascript-function-objects>
#[derive(Finalize, Clone)]
pub struct Function {
    /// Kind, this *may* not be needed but will keep for now
    pub kind: ObjectKind,
    /// Internal Slots
    pub internal_slots: Box<HashMap<String, Value>>,
    /// Properties
    pub properties: Box<HashMap<String, Property>>,
    /// Function Kind
    pub function_kind: FunctionKind,
    /// is constructor??
    pub is_constructor: bool,
    /// Function Body
    pub body: FunctionBody,
    /// Formal Paramaters
    pub params: Vec<FormalParameter>,
    /// This Mode
    pub this_mode: ThisMode,
    /// Reference to the current Environment Record
    pub environment: Environment,
}

impl Function {
    /// This will create an ordinary function object
    ///
    /// <https://tc39.es/ecma262/#sec-ordinaryfunctioncreate>
    pub fn create_ordinary(
        proto: Value,
        parameter_list: Vec<FormalParameter>,
        body: FunctionBody,
        this_mode: ThisMode,
        realm: &mut Realm,
        mut kind: FunctionKind,
    ) -> Function {
        let needs_construct: bool;
        match kind {
            FunctionKind::Normal => needs_construct = true,
            FunctionKind::NonConstructor => {
                needs_construct = false;
                kind = FunctionKind::Normal;
            }
            _ => needs_construct = false,
        }

        // Create length property and set it's value
        let length_property = Property::new()
            .writable(false)
            .enumerable(false)
            .configurable(true)
            .value(to_value(parameter_list.len()));

        let mut func = Function {
            kind: ObjectKind::Function,
            internal_slots: Box::new(HashMap::new()),
            properties: Box::new(HashMap::new()),
            function_kind: kind,
            is_constructor: needs_construct,
            body,
            environment: realm.environment.get_current_environment().clone(),
            params: parameter_list,
            this_mode,
        };

        func.set_internal_slot("extensible", to_value(true));
        func.set_internal_slot(PROTOTYPE, to_value(proto.clone()));
        func.set_internal_slot("home_object", to_value(undefined()));

        func.define_own_property(String::from("length"), length_property);
        func
    }
}

unsafe impl gc::Trace for Function {
    custom_trace!(this, mark(&this.properties));
}

impl ObjectInternalMethods for Function {
    /// <https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-setprototypeof-v>
    fn set_prototype_of(&mut self, val: Value) -> bool {
        debug_assert!(val.is_object() || val.is_null());
        let current = self.get_internal_slot(PROTOTYPE);
        if current == val {
            return true;
        }
        let extensible = self.get_internal_slot("extensible");
        if extensible.is_null() {
            return false;
        }
        let mut p = val.clone();
        let mut done = false;
        while !done {
            if p.is_null() {
                done = true
            } else if same_value(&to_value(self.clone()), &p, false) {
                return false;
            } else {
                p = p.get_internal_slot(PROTOTYPE);
            }
        }
        self.set_internal_slot(PROTOTYPE, val);
        true
    }

    /// <https://tc39.es/ecma262/#sec-ordinary-object-internal-methods-and-internal-slots-getownproperty-p>
    /// The specification returns a Property Descriptor or Undefined. These are 2 separate types and we can't do that here.
    fn get_own_property(&self, prop: &Value) -> Property {
        debug_assert!(Property::is_property_key(prop));
        match self.properties.get(&prop.to_string()) {
            // If O does not have an own property with key P, return undefined.
            // In this case we return a new empty Property
            None => Property::default(),
            Some(ref v) => {
                let mut d = Property::default();
                if v.is_data_descriptor() {
                    d.value = v.value.clone();
                    d.writable = v.writable;
                } else {
                    debug_assert!(v.is_accessor_descriptor());
                    d.get = v.get.clone();
                    d.set = v.set.clone();
                }
                d.enumerable = v.enumerable;
                d.configurable = v.configurable;
                d
            }
        }
    }

    /// Insert property into properties hashmap
    fn insert_property(&mut self, name: String, p: Property) {
        self.properties.insert(name, p);
    }

    /// Remove property from properties hashmap
    fn remove_property(&mut self, name: &str) {
        self.properties.remove(&name.to_string());
    }

    /// Utility function to get an immutable internal slot or Null
    fn get_internal_slot(&self, name: &str) -> Value {
        match self.internal_slots.get(name) {
            Some(v) => v.clone(),
            None => Gc::new(ValueData::Null),
        }
    }

    /// Utility function to set an internal slot
    fn set_internal_slot(&mut self, name: &str, val: Value) {
        self.internal_slots.insert(name.to_string(), val);
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        for (key, val) in self.properties.iter() {
            write!(
                f,
                "{}: {}",
                key,
                val.value
                    .as_ref()
                    .unwrap_or(&Gc::new(ValueData::Undefined))
                    .clone()
            )?;
        }
        write!(f, "}}")
    }
}

/// Function Prototype
/// <https://tc39.es/ecma262/#sec-properties-of-the-function-prototype-object>
pub fn create_function_prototype() {
    let mut function_prototype: Object = Object::default();
    // Set Kind to function
    function_prototype.kind = ObjectKind::Function;
}
