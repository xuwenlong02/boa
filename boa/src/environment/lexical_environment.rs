//! # Lexical Environment
//!
//! <https://tc39.es/ecma262/#sec-lexical-environment-operations>
//!
//! The following operations are used to operate upon lexical environments
//! This is the entrypoint to lexical environments.
//!

use crate::{
    builtins::value::{Value, ValueData},
    environment::{
        declarative_environment_record::DeclarativeEnvironmentRecord,
        function_environment_record::{BindingStatus, FunctionEnvironmentRecord},
        global_environment_record::GlobalEnvironmentRecord,
        object_environment_record::ObjectEnvironmentRecord,
        EnvironmentRecord,
    },
};
use gc::Gc;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet, VecDeque},
    error, fmt,
    rc::{Rc, Weak},
};

// /// Environments are wrapped in a Box and then in a reference counted pointer
// pub type Environment = Rc<Box<dyn EnvironmentRecordTrait>>;

/// Give each environment an easy way to declare its own type
/// This helps with comparisons
#[derive(Debug, Clone, Copy)]
pub enum EnvironmentType {
    Declarative,
    Function,
    Global,
    Object,
}

/// The scope of a given variable
#[derive(Debug, Clone, Copy)]
pub enum VariableScope {
    /// The variable declaration is scoped to the current block (`let` and `const`)
    Block,
    /// The variable declaration is scoped to the current function (`var`)
    Function,
}

#[derive(Debug)]
pub struct LexicalEnvironment {
    environment_stack: VecDeque<Rc<RefCell<dyn EnvironmentRecord>>>,
}

/// An error that occurred during lexing or compiling of the source input.
#[derive(Debug, Clone)]
pub struct EnvironmentError {
    details: String,
}

impl EnvironmentError {
    pub fn new(msg: &str) -> Self {
        Self {
            details: msg.to_string(),
        }
    }
}

impl fmt::Display for EnvironmentError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl error::Error for EnvironmentError {
    fn description(&self) -> &str {
        &self.details
    }

    fn cause(&self) -> Option<&dyn error::Error> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

impl LexicalEnvironment {
    pub fn new(global: Value) -> Self {
        let global_env = new_global_environment(global.clone(), global);
        let mut lexical_env = Self {
            environment_stack: VecDeque::new(),
        };

        // lexical_env.push(global_env);
        lexical_env.environment_stack.push_back(global_env);
        lexical_env
    }

    pub fn push(&mut self, env: Rc<RefCell<dyn EnvironmentRecord>>) {
        let current_env = Rc::downgrade(self.get_current_environment());
        env.borrow_mut().set_outer_environment(current_env);
        self.environment_stack.push_back(env);
    }

    pub fn pop(&mut self) -> Option<Rc<RefCell<dyn EnvironmentRecord>>> {
        self.environment_stack.pop_back()
    }

    pub fn environments(&self) -> impl Iterator<Item = Weak<RefCell<dyn EnvironmentRecord>>> {
        std::iter::successors(
            Some(Rc::downgrade(&self.get_current_environment_ref())),
            |env| {
                env.upgrade()
                    .expect("outer env disappeared")
                    .borrow()
                    .get_outer_environment()
            },
        )
    }

    pub fn get_global_object(&self) -> Option<Value> {
        self.environment_stack
            .get(0)
            .expect("could not get first environment")
            .borrow()
            .get_global_object()
    }

    pub fn create_mutable_binding(&mut self, name: String, deletion: bool, scope: VariableScope) {
        match scope {
            VariableScope::Block => self
                .get_current_environment()
                .borrow_mut()
                .create_mutable_binding(name, deletion),
            VariableScope::Function => {
                // Find the first function or global environment (from the top of the stack)
                let env = self
                    .environments()
                    .find(|env| {
                        match env
                            .upgrade()
                            .expect("outer env disappeared")
                            .borrow_mut()
                            .get_environment_type()
                        {
                            EnvironmentType::Function | EnvironmentType::Global => true,
                            _ => false,
                        }
                    })
                    .expect("No function or global environment");

                env.upgrade()
                    .expect("outer env disappeared")
                    .borrow_mut()
                    .create_mutable_binding(name, deletion);
            }
        }
    }

    pub fn create_immutable_binding(
        &mut self,
        name: String,
        deletion: bool,
        scope: VariableScope,
    ) -> bool {
        match scope {
            VariableScope::Block => self
                .get_current_environment()
                .borrow_mut()
                .create_immutable_binding(name, deletion),
            VariableScope::Function => {
                // Find the first function or global environment (from the top of the stack)
                let env = self
                    .environments()
                    .find(|env| {
                        match env
                            .upgrade()
                            .expect("outer env disappeared")
                            .borrow_mut()
                            .get_environment_type()
                        {
                            EnvironmentType::Function | EnvironmentType::Global => true,
                            _ => false,
                        }
                    })
                    .expect("No function or global environment");

                #[allow(clippy::let_and_return)]
                // FIXME need to assign result to a variable to avoid borrow checker error
                // (borrowed value `env` does not live long enough)
                let b = env
                    .upgrade()
                    .expect("outer env disappeared")
                    .borrow_mut()
                    .create_immutable_binding(name, deletion);
                b
            }
        }
    }

    pub fn set_mutable_binding(&mut self, name: &str, value: Value, strict: bool) {
        // Find the first environment which has the given binding
        let env = self
            .environments()
            .find(|env| {
                env.upgrade()
                    .expect("outer env disappeared")
                    .borrow()
                    .has_binding(name)
            })
            .expect("Binding does not exists"); // TODO graceful error handling

        env.upgrade()
            .expect("outer env disappeared")
            .borrow_mut()
            .set_mutable_binding(name, value, strict);
    }

    pub fn initialize_binding(&mut self, name: &str, value: Value) {
        // Find the first environment which has the given binding
        let env = self
            .environments()
            .find(|env| {
                env.upgrade()
                    .expect("outer env disappeared")
                    .borrow()
                    .has_binding(name)
            })
            .expect("Binding does not exists"); // TODO graceful error handling

        env.upgrade()
            .expect("outer env disappeared")
            .borrow_mut()
            .initialize_binding(name, value);
    }

    /// get_current_environment_ref is used when you only need to borrow the environment
    /// (you only need to add a new variable binding, or you want to fetch a value)
    pub fn get_current_environment_ref(&self) -> &Rc<RefCell<dyn EnvironmentRecord>> {
        let index = self.environment_stack.len().wrapping_sub(1);
        &self
            .environment_stack
            .get(index)
            .expect("Could not get current environment")
    }

    /// When neededing to clone an environment (linking it with another environnment)
    /// cloning is more suited. The GC will remove the env once nothing is linking to it anymore
    pub fn get_current_environment(&mut self) -> &mut Rc<RefCell<dyn EnvironmentRecord>> {
        self.environment_stack
            .back_mut()
            .expect("Could not get mutable reference to back object")
    }

    pub fn has_binding(&self, name: &str) -> bool {
        self.environments().any(|env| {
            env.upgrade()
                .expect("outer env disappeared")
                .borrow()
                .has_binding(name)
        })
    }

    pub fn get_binding_value(&self, name: &str) -> Value {
        self.environments()
            .find(|env| {
                env.upgrade()
                    .expect("outer env disappeared")
                    .borrow()
                    .has_binding(name)
            })
            .map(|env| {
                env.upgrade()
                    .expect("outer env disappeared")
                    .borrow()
                    .get_binding_value(name, false)
            })
            .unwrap_or_else(|| Gc::new(ValueData::Undefined))
    }
}

pub fn new_declarative_environment(
    env: Option<Weak<RefCell<dyn EnvironmentRecord>>>,
) -> Rc<RefCell<dyn EnvironmentRecord>> {
    Rc::new(RefCell::new(DeclarativeEnvironmentRecord {
        env_rec: HashMap::new(),
        outer_env: env,
    }))
}

pub fn new_function_environment(
    f: Value,
    new_target: Value,
    outer: Option<Weak<RefCell<dyn EnvironmentRecord>>>,
) -> Rc<RefCell<dyn EnvironmentRecord>> {
    debug_assert!(f.is_function());
    debug_assert!(new_target.is_object() || new_target.is_undefined());
    Rc::new(RefCell::new(FunctionEnvironmentRecord {
        env_rec: HashMap::new(),
        function_object: f,
        this_binding_status: BindingStatus::Uninitialized, // hardcoding to unitialized for now until short functions are properly supported
        home_object: Gc::new(ValueData::Undefined),
        new_target,
        outer_env: outer, // this will come from Environment set as a private property of F - https://tc39.es/ecma262/#sec-ecmascript-function-objects
        this_value: Gc::new(ValueData::Undefined), // TODO: this_value should start as an Option as its not always there to begin with
    }))
}

pub fn new_object_environment(
    object: Value,
    environment: Option<Weak<RefCell<dyn EnvironmentRecord>>>,
) -> Rc<RefCell<dyn EnvironmentRecord>> {
    Rc::new(RefCell::new(ObjectEnvironmentRecord {
        bindings: object,
        outer_env: environment,
        /// Object Environment Records created for with statements (13.11)
        /// can provide their binding object as an implicit this value for use in function calls.
        /// The capability is controlled by a withEnvironment Boolean value that is associated
        /// with each object Environment Record. By default, the value of withEnvironment is false
        /// for any object Environment Record.
        with_environment: false,
    }))
}

pub fn new_global_environment(
    global: Value,
    this_value: Value,
) -> Rc<RefCell<dyn EnvironmentRecord>> {
    let obj_rec = ObjectEnvironmentRecord {
        bindings: global,
        outer_env: None,
        /// Object Environment Records created for with statements (13.11)
        /// can provide their binding object as an implicit this value for use in function calls.
        /// The capability is controlled by a withEnvironment Boolean value that is associated
        /// with each object Environment Record. By default, the value of withEnvironment is false
        /// for any object Environment Record.
        with_environment: false,
    };

    let dcl_rec = DeclarativeEnvironmentRecord {
        env_rec: HashMap::new(),
        outer_env: None,
    };

    Rc::new(RefCell::new(GlobalEnvironmentRecord {
        object_record: obj_rec,
        global_this_binding: this_value,
        declarative_record: dcl_rec,
        var_names: HashSet::new(),
    }))
}

#[cfg(test)]
mod tests {
    use crate::exec;

    #[test]
    fn let_is_blockscoped() {
        let scenario = r#"
          {
            let bar = "bar";
          }
          bar == undefined;
        "#;

        assert_eq!(&exec(scenario), "true");
    }

    #[test]
    fn const_is_blockscoped() {
        let scenario = r#"
          {
            const bar = "bar";
          }
          bar == undefined;
        "#;

        assert_eq!(&exec(scenario), "true");
    }

    #[test]
    fn var_not_blockscoped() {
        let scenario = r#"
          {
            var bar = "bar";
          }
          bar == "bar";
        "#;

        assert_eq!(&exec(scenario), "true");
    }

    #[test]
    fn set_outer_var_in_blockscope() {
        let scenario = r#"
          var bar;
          {
            bar = "foo";
          }
          bar == "foo";
        "#;

        assert_eq!(&exec(scenario), "true");
    }

    #[test]
    fn set_outer_let_in_blockscope() {
        let scenario = r#"
          let bar;
          {
            bar = "foo";
          }
          bar == "foo";
        "#;

        assert_eq!(&exec(scenario), "true");
    }
}
