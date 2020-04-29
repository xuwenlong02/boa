use crate::{
    builtins::{
        array,
        object::{Object, ObjectInternalMethods, ObjectKind},
        property::Property,
        value::{to_value, undefined, ResultValue, Value, ValueData},
    },
    environment::lexical_environment::{new_function_environment, Environment},
    exec::Executor,
    syntax::ast::node::{FormalParameter, Node},
    Interpreter,
};
use gc::{unsafe_empty_trace, Gc, Trace as TraceTrait};
use gc_derive::{Finalize, Trace};
use std::fmt::{self, Debug};

/// _fn(this, arguments, ctx) -> ResultValue_ - The signature of a built-in function
pub type NativeFunctionData = fn(&Value, &[Value], &mut Interpreter) -> ResultValue;

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

/// Signal what sort of function this is
#[derive(Clone, Trace, Finalize)]
pub enum FunctionKind {
    BuiltIn,
    Ordinary,
}

// This is indeed safe, but we need to mark this as an empty trace because
// neither NativeFunctionData nor Node hold any GC'd objects, but Gc doesn't know that
// So we need to signal it manually.
// rust-gc does not have a Trace impl for fn(_, _, _)
// https://github.com/Manishearth/rust-gc/blob/master/gc/src/trace.rs
unsafe impl TraceTrait for FunctionBody {
    unsafe_empty_trace!();
}

/// Boa representation of a Function Object.   
/// <https://tc39.es/ecma262/#sec-ecmascript-function-objects>
#[derive(Trace, Finalize, Clone)]
pub struct Function {
    /// Call/Construct Function body
    pub body: FunctionBody,
    /// Formal Paramaters
    pub params: Vec<FormalParameter>,
    /// This Mode
    pub this_mode: ThisMode,
    /// Function kind
    pub kind: FunctionKind,
    // Environment, built-in functions don't need Environments
    pub environment: Option<Environment>,
}

impl Function {
    /// This will create an ordinary function object
    ///
    /// <https://tc39.es/ecma262/#sec-ordinaryfunctioncreate>
    pub fn create_ordinary(
        proto: Value,
        parameter_list: Vec<FormalParameter>,
        scope: Environment,
        body: FunctionBody,
        this_mode: ThisMode,
    ) -> Function {
        // Create length property and set it's value
        let length_property = Property::new()
            .writable(false)
            .enumerable(false)
            .configurable(true)
            .value(to_value(parameter_list.len()));

        let mut func = Function {
            body,
            environment: Some(scope),
            params: parameter_list,
            kind: FunctionKind::Ordinary,
            this_mode,
        };

        func
    }

    /// This will create a built-in function object
    ///
    /// <https://tc39.es/ecma262/#sec-createbuiltinfunction>
    pub fn create_builtin(parameter_list: Vec<FormalParameter>, body: FunctionBody) -> Function {
        // Create length property and set it's value
        let length_property = Property::new()
            .writable(false)
            .enumerable(false)
            .configurable(true)
            .value(to_value(parameter_list.len()));

        let func: Function = Function {
            body,
            params: parameter_list,
            this_mode: ThisMode::NonLexical,
            kind: FunctionKind::BuiltIn,
            environment: None,
        };

        func
    }

    /// This will handle calls for both ordinary and built-in functions
    ///
    /// <https://tc39.es/ecma262/#sec-prepareforordinarycall>
    /// <https://tc39.es/ecma262/#sec-ecmascript-function-objects-call-thisargument-argumentslist>
    pub fn call(
        &self,
        this: &Value, // represents a pointer to this function object wrapped in a GC (not a `this` JS object)
        args_list: &Vec<Value>,
        interpreter: &mut Interpreter,
    ) -> ResultValue {
        // Is this a builtin function?
        if let FunctionKind::BuiltIn = self.kind {
            return self.call_builtin(this, args_list, interpreter);
        };

        // Create a new Function environment who's parent is set to the scope of the function declaration (self.environment)
        // <https://tc39.es/ecma262/#sec-prepareforordinarycall>
        let local_env = new_function_environment(
            this.clone(),
            undefined(),
            Some(self.environment.as_ref().unwrap().clone()),
        );

        // Add argument bindings to the function environment
        for i in 0..self.params.len() {
            let param = self.params.get(i).expect("Could not get param");
            // Rest Parameters
            if param.is_rest_param {
                self.add_rest_param(param, i, args_list, interpreter, &local_env);
                break;
            }

            let value = args_list.get(i).expect("Could not get value");
            self.add_arguments_to_environment(param, value.clone(), &local_env);
        }

        // Add arguments object
        let arguments_obj = create_unmapped_arguments_object(args_list);
        local_env
            .borrow_mut()
            .create_mutable_binding("arguments".to_string(), false);
        local_env
            .borrow_mut()
            .initialize_binding("arguments", arguments_obj);

        interpreter.realm.environment.push(local_env);

        // Call body should be set before reaching here
        let result = match &self.body {
            FunctionBody::BuiltIn(func) => func(this, args_list, interpreter),
            FunctionBody::Ordinary(ref body) => interpreter.run(body),
        };

        // local_env gets dropped here, its no longer needed
        interpreter.realm.environment.pop();
        result
    }

    /// Call a builtin function
    fn call_builtin(
        &self,
        this: &Value, // represents a pointer to this function object wrapped in a GC (not a `this` JS object)
        args_list: &Vec<Value>,
        interpreter: &mut Interpreter,
    ) -> ResultValue {
        match &self.body {
            FunctionBody::BuiltIn(func) => func(this, args_list, interpreter),
            FunctionBody::Ordinary(_) => panic!("Ordinary function expected"),
        }
    }

    // Adds the final rest parameters to the Environment as an array
    fn add_rest_param(
        &self,
        param: &FormalParameter,
        index: usize,
        args_list: &Vec<Value>,
        interpreter: &mut Interpreter,
        local_env: &Environment,
    ) {
        // Create array of values
        let array = array::new_array(interpreter).unwrap();
        array::add_to_array_object(&array, &args_list[index..]).unwrap();

        // Create binding
        local_env
            .borrow_mut()
            .create_mutable_binding(param.name.clone(), false);

        // Set Binding to value
        local_env
            .borrow_mut()
            .initialize_binding(&param.name, array);
    }

    // Adds an argument to the environment
    fn add_arguments_to_environment(
        &self,
        param: &FormalParameter,
        value: Value,
        local_env: &Environment,
    ) {
        // Create binding
        local_env
            .borrow_mut()
            .create_mutable_binding(param.name.clone(), false);

        // Set Binding to value
        local_env
            .borrow_mut()
            .initialize_binding(&param.name, value.clone());
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        write!(f, "[Not implemented]")?;
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
