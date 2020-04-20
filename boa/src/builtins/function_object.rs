use crate::{
    builtins::{
        array,
        object::{Object, ObjectInternalMethods, ObjectKind, PROTOTYPE},
        property::Property,
        value::{same_value, to_value, undefined, ResultValue, Value, ValueData},
    },
    environment::lexical_environment::Environment,
    exec::Executor,
    syntax::ast::node::{FormalParameter, Node},
    Interpreter,
};

use gc::{unsafe_empty_trace, Gc, Trace as TraceTrait};
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
///
/// Arrow functions don't define a `this` and thus are lexical, `function`s do define a this and thus are NonLexical
#[derive(Trace, Finalize, Debug, Clone)]
pub enum ThisMode {
    Lexical,
    NonLexical,
}

/// FunctionBody is specific to this interpreter, it will either be Rust code or JavaScript code (AST Node)
#[derive(Clone, Finalize)]
pub enum FunctionBody {
    BuiltIn(NativeFunctionData),
    Ordinary(Node),
}

// This is indeed safe, but we need to mark this as an empty trace because
// NativeFunctionData doesn't hold any GC'd objects, but Gc doesn't know that
// So we need to signal it manually
unsafe impl TraceTrait for FunctionBody {
    unsafe_empty_trace!();
}

/// Boa representation of a Function Object.   
/// <https://tc39.es/ecma262/#sec-ecmascript-function-objects>
#[derive(Trace, Finalize, Clone)]
pub struct Function {
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
    // Environment
    pub environment: Option<Environment>,
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
            internal_slots: Box::new(HashMap::new()),
            properties: Box::new(HashMap::new()),
            function_kind: kind,
            is_constructor: needs_construct,
            body,
            environment: None,
            params: parameter_list,
            this_mode,
        };

        func.set_internal_slot("extensible", to_value(true));
        func.set_internal_slot(PROTOTYPE, to_value(proto.clone()));
        func.set_internal_slot("home_object", to_value(undefined()));

        func.define_own_property(String::from("length"), length_property);
        func
    }

    /// Sets the current environment on this function object
    ///
    /// This should be set after creating struct instance.
    /// Environment can't be an internal slot due to it not being a JSValue
    pub fn set_environment(&mut self, env: Environment) {
        self.environment = Some(env);
    }

    /// Fetches the current environment on this function.
    ///
    /// The environment should be a Function Declarative Record
    ///
    /// It should exist, if not it will panic
    pub fn get_environment(&self) -> Environment {
        match self.environment.clone() {
            Some(v) => v,
            None => panic!("No environment set on Function!"),
        }
    }

    /// This will handle calls for both ordinary and built-in functions
    ///
    /// <https://tc39.es/ecma262/#sec-ecmascript-function-objects-call-thisargument-argumentslist>
    pub fn call(
        &self,
        this: &Value,
        args_list: &Vec<Value>,
        interpreter: &mut Interpreter,
    ) -> ResultValue {
        match self.body {
            FunctionBody::BuiltIn(func) => func(this, args_list, interpreter),
            FunctionBody::Ordinary(ref body) => {
                // Add argument bindings to the function environment
                for i in 0..self.params.len() {
                    let param = self.params.get(i).expect("Could not get param");
                    // Rest Parameters
                    if param.is_rest_param {
                        self.add_rest_param(param, i, args_list, interpreter);
                        break;
                    }

                    let value = args_list.get(i).expect("Could not get value");

                    self.add_arguments_to_environment(param, value.clone());
                }

                // Add arguments object
                let arguments_obj = create_unmapped_arguments_object(args_list);
                self.get_environment()
                    .borrow_mut()
                    .create_mutable_binding("arguments".to_string(), false);
                self.get_environment()
                    .borrow_mut()
                    .initialize_binding("arguments", arguments_obj);

                interpreter.run(body)
            }
        }
    }

    // Adds the final rest parameters to the Environment as an array
    fn add_rest_param(
        &self,
        param: &FormalParameter,
        index: usize,
        args_list: &Vec<Value>,
        interpreter: &mut Interpreter,
    ) {
        // Create array of values
        let array = array::new_array(interpreter).unwrap();
        array::add_to_array_object(&array, &args_list[index..]).unwrap();

        // Create binding
        self.get_environment()
            .borrow_mut()
            .create_mutable_binding(param.name.clone(), false);

        // Set Binding to value
        self.get_environment()
            .borrow_mut()
            .initialize_binding(&param.name, array);
    }

    // Adds an argument to the environment
    fn add_arguments_to_environment(&self, param: &FormalParameter, value: Value) {
        // Create binding
        self.get_environment()
            .borrow_mut()
            .create_mutable_binding(param.name.clone(), false);

        // Set Binding to value
        self.get_environment()
            .borrow_mut()
            .initialize_binding(&param.name, value.clone());
    }
}

// unsafe impl gc::Trace for Function {
//     custom_trace!(this, mark(&this.is_constructor));
// }

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
    // Set Kind to function (for historical & compatibility reasons)
    // https://tc39.es/ecma262/#sec-properties-of-the-function-prototype-object
    function_prototype.kind = ObjectKind::Function;
}

/// Arguments
/// https://tc39.es/ecma262/#sec-createunmappedargumentsobject
pub fn create_unmapped_arguments_object(arguments_list: &Vec<Value>) -> Value {
    let len = arguments_list.len();
    let mut obj = Object::default();
    obj.set_internal_slot("ParameterMap", Gc::new(ValueData::Undefined));
    // Set length
    let mut length = Property::default();
    length = length.writable(true).value(to_value(len));
    // Define length as a property
    obj.define_own_property("length".to_string(), length);
    let mut index: usize = 0;
    while index < len {
        let val = arguments_list.get(index).expect("Could not get argument");
        let mut prop = Property::default();
        prop = prop
            .value(val.clone())
            .enumerable(true)
            .writable(true)
            .configurable(true);

        obj.properties.insert(index.to_string(), prop);
        index += 1;
    }

    to_value(obj)
}
