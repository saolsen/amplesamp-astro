use std::collections::{HashMap, HashSet};
use std::fmt::Display;

use rand::distributions::Alphanumeric;
use rand::{Rng, SeedableRng};
use serde::Serialize;

use crate::bytecode::{FieldName, Program, TypeName, Value, VariableName};
use crate::error::Error;

#[derive(Debug, Clone, Copy, Serialize)]
pub struct Tick(usize);

impl Tick {
    pub fn new() -> Self {
        Self(0)
    }
    fn tick(&mut self) {
        self.0 += 1;
    }
}

// todo: need event streams for variables and the stack to show that info during
// debugging too.
// todo: put the disassembly in here too.
// This is tied to the vm because it shows the values of the interned strings.
#[derive(Debug, Serialize)]
#[serde(tag = "type")]
pub enum DisplayValue<'a> {
    Nil,
    Int {
        value: i64,
    },
    Dec {
        value: f64,
    },
    String {
        value: &'a str,
    },
    Bool {
        value: bool,
    },
    // Why is tag a display value?
    Tag {
        value: &'a str,
    },
    Object {
        object_type: &'a str,
        fields: HashMap<&'a str, DisplayValue<'a>>,
    },
}

impl<'a> DisplayValue<'a> {
    fn object(object_type: &'a str, fields: HashMap<&'a str, DisplayValue<'a>>) -> Self {
        Self::Object {
            object_type,
            fields,
        }
    }
}

impl<'a> Display for DisplayValue<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DisplayValue::Nil => write!(f, "nil"),
            DisplayValue::Int { value } => write!(f, "{}", value),
            DisplayValue::Dec { value } => write!(f, "{}", value),
            DisplayValue::Tag { value } => write!(f, "{}", value),
            DisplayValue::Object {
                object_type,
                fields,
            } => {
                write!(f, "{} {{", object_type)?;
                for (name, value) in fields {
                    write!(f, " {}: {},", name, value)?;
                }
                write!(f, " }}")
            }
            DisplayValue::String { value } => write!(f, "\"{}\"", value),
            DisplayValue::Bool { value } => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct TickValue<'a> {
    pub tick: Tick,
    pub value: DisplayValue<'a>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TickOutput {
    pub tick: Tick,
    pub value: String,
}

#[derive(Debug, Serialize)]
pub struct TickError {
    pub tick: Tick,
    pub error: Error,
}

#[derive(Debug, Serialize)]
pub struct Results<'a> {
    pub last_tick: Tick,
    pub objects: HashMap<&'a str, Vec<TickValue<'a>>>,
    pub output: Vec<TickOutput>,
    pub errors: Vec<TickError>,
}

impl<'a> Results<'a> {
    pub fn default() -> Self {
        Self {
            last_tick: Tick::new(),
            objects: HashMap::new(),
            output: Vec::new(),
            errors: Vec::new(),
        }
    }
}

pub struct Vm {
    program: Program,

    globals: HashMap<VariableName, Value>,
    // Starting simple, no deletions
    objects: HashMap<TypeName, Vec<(Tick, HashMap<FieldName, Value>)>>,
    output: Vec<TickOutput>,
    stack: Vec<Value>,
    ip: usize,
    tick: Tick,
}

impl Vm {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            globals: HashMap::new(),
            objects: HashMap::new(),
            stack: Vec::new(),
            ip: 0,
            tick: Tick::new(),
            output: Vec::new(),
        }
    }

    pub fn run(&mut self) -> Results {
        let run = self.try_run();
        let mut results = Results {
            last_tick: self.tick,
            objects: HashMap::new(),
            output: self.output.clone(),
            errors: Vec::new(),
        };
        // display objects
        for (type_name, objects) in &self.objects {
            let type_name = self.program.type_name(type_name);
            let mut display_objects = vec![];
            for (tick, fields) in objects {
                let mut display_fields = HashMap::new();
                for (field_name, value) in fields {
                    let field_name = self.program.field_name(field_name);
                    let display_value = self.display(value);
                    display_fields.insert(field_name, display_value);
                }
                display_objects.push(TickValue {
                    tick: *tick,
                    value: DisplayValue::object(type_name, display_fields),
                });
            }
            results.objects.insert(type_name, display_objects);
        }
        // add error
        if let Err(e) = run {
            results.errors.push(TickError {
                tick: self.tick,
                error: e,
            });
        };
        results
    }

    pub fn stack_pop(&mut self) -> Result<Value, Error> {
        if let Some(value) = self.stack.pop() {
            Ok(value)
        } else {
            let loc = self.program.code_locs[self.ip].clone();
            Err(Error::Runtime {
                line: loc.line,
                column: loc.col,
                message: "stack underflow".to_string(),
            })
        }
    }

    pub fn globals_get(&self, var_name: &VariableName) -> Result<Value, Error> {
        if let Some(val) = self.globals.get(var_name) {
            Ok(val.clone())
        } else {
            let loc = self.program.code_locs[self.ip].clone();
            let name = self.program.variable_name(var_name);
            Err(Error::Runtime {
                line: loc.line,
                column: loc.col,
                message: format!("global variable {} not found", name),
            })
        }
    }

    pub fn display(&self, value: &Value) -> DisplayValue {
        match value {
            Value::Nil => DisplayValue::Nil,
            Value::Int(i) => DisplayValue::Int { value: *i },
            Value::Dec(d) => DisplayValue::Dec { value: *d },
            Value::Tag(field_name) => {
                let name = self.program.field_name(field_name);
                DisplayValue::Tag { value: name }
            }
            Value::Object(object_type, id) => {
                let object_type_name = self.program.type_name(object_type);
                let (_tick, fields) = &self.objects[object_type][*id];
                let mut display_fields = HashMap::new();
                for (field_name, value) in fields {
                    let field_name = self.program.field_name(field_name);
                    let value = self.display(value);
                    display_fields.insert(field_name, value);
                }
                DisplayValue::Object {
                    object_type: object_type_name,
                    fields: display_fields,
                }
            }
            Value::Bool(b) => DisplayValue::Bool { value: *b },
            Value::String(s) => {
                let s = self.program.string(s);
                DisplayValue::String { value: s }
            }
        }
    }

    pub fn try_run(&mut self) -> Result<(), Error> {
        loop {
            self.tick.tick();
            let loc = self.program.code_locs[self.ip].clone();
            let op = self.program.code[self.ip];
            use crate::bytecode::Op;
            match op {
                Op::Halt => {
                    break;
                }
                Op::Return => (),
                Op::Constant(i) => {
                    let val = &self.program.constants[i];
                    self.stack.push(val.clone());
                }
                Op::Print => {
                    let val = self.stack_pop()?;
                    let display_val = self.display(&val);
                    self.output.push(TickOutput {
                        tick: self.tick,
                        value: format!("{}", display_val),
                    });
                }
                Op::Pop => {
                    _ = self.stack_pop()?;
                }
                Op::DefineGlobal(var_name) => {
                    let val = self.stack_pop()?;
                    self.globals.insert(var_name, val);
                }
                // todo: assign to more than just variables, specifically fields
                Op::AssignVar(var_name) => {
                    let val = self.stack_pop()?;
                    // todo: check if var is defined otherwise it's a runtime error
                    self.globals.insert(var_name, val.clone());
                    // put value back on the stack
                    self.stack.push(val);
                }
                Op::AssignField(field_name) => {
                    let lhs = self.stack_pop()?;
                    let rhs = self.stack_pop()?;
                    if let Value::Object(typ, id) = lhs {
                        self.objects.entry(typ).and_modify(|objs| {
                            objs[id].1.insert(field_name, rhs);
                        });
                    } else {
                        return Err(Error::Runtime {
                            line: loc.line,
                            column: loc.col,
                            message: format!(
                                "Can only assign field {} on an object",
                                self.program.field_name(&field_name),
                            ),
                        });
                    }
                }
                Op::Read(var_name) => {
                    let val = self.globals_get(&var_name)?;
                    self.stack.push(val.clone());
                }
                Op::Get(field_name) => {
                    let val = self.stack_pop()?;
                    if let Value::Object(typ, id) = val {
                        let (_, ref fields) = self.objects.get(&typ).unwrap()[id];
                        if let Some(val) = fields.get(&field_name) {
                            self.stack.push(val.clone());
                        } else {
                            return Err(Error::Runtime {
                                line: loc.line,
                                column: loc.col,
                                message: format!(
                                    "no field {} on {}",
                                    self.program.field_name(&field_name),
                                    self.program.type_name(&typ)
                                ),
                            });
                        }
                    } else {
                        return Err(Error::Runtime {
                            line: loc.line,
                            column: loc.col,
                            message: "expected object".to_string(),
                        });
                    }
                }
                Op::Tag(field_name) => {
                    self.stack.push(Value::Tag(field_name));
                }
                Op::Create(type_name) => {
                    if !self.program.types.contains_key(&type_name) {
                        let type_name = self.program.type_name(&type_name);
                        return Err(Error::Runtime {
                            message: format!("Type {} not defined", type_name),
                            line: loc.line,
                            column: loc.col,
                        });
                    }
                    let typedef = self.program.types.get(&type_name).unwrap();
                    let mut needed_fields: HashSet<FieldName> =
                        typedef.fields.keys().cloned().collect();
                    let mut object = HashMap::new();
                    // collect fields from the stack
                    loop {
                        let val = self.stack.pop();
                        match val {
                            Some(Value::Tag(field_name)) => {
                                let val = self.stack_pop()?;
                                if !needed_fields.contains(&field_name) {
                                    let type_name = self.program.type_name(&type_name);
                                    let field_name = self.program.field_name(&field_name);
                                    return Err(Error::Runtime {
                                        message: format!(
                                            "Unknown field {} for type {}",
                                            type_name, field_name
                                        ),
                                        line: loc.line,
                                        column: loc.col,
                                    });
                                }
                                needed_fields.remove(&field_name);
                                object.insert(field_name, val);
                            }
                            None => break,
                            Some(v) => {
                                self.stack.push(v);
                                break;
                            }
                        }
                    }
                    // check that all fields were provided
                    if !needed_fields.is_empty() {
                        let type_name = self.program.type_name(&type_name);
                        let missing_fields = needed_fields
                            .iter()
                            .map(|field_name| self.program.field_name(field_name).to_owned())
                            .collect::<Vec<String>>()
                            .join(", ");
                        return Err(Error::Runtime {
                            message: format!(
                                "Missing fields [{}] for type {}",
                                missing_fields, type_name
                            ),
                            line: loc.line,
                            column: loc.col,
                        });
                    }
                    // Save the object
                    let objs = self.objects.entry(type_name).or_default();
                    objs.push((self.tick, object));
                    // Put the object on the stack
                    self.stack.push(Value::Object(type_name, objs.len() - 1));
                }
                Op::Query(type_name) => {
                    if !self.program.types.contains_key(&type_name) {
                        let type_name = self.program.type_name(&type_name);
                        return Err(Error::Runtime {
                            message: format!("Type {} not defined", type_name),
                            line: loc.line,
                            column: loc.col,
                        });
                    }
                    let typedef = self.program.types.get(&type_name).unwrap();
                    let mut needed_fields: HashSet<FieldName> =
                        typedef.fields.keys().cloned().collect();
                    let mut object = HashMap::new();
                    // collect fields from the stack
                    loop {
                        let val = self.stack.pop();
                        match val {
                            Some(Value::Tag(field_name)) => {
                                let val = self.stack_pop()?;
                                if !needed_fields.contains(&field_name) {
                                    let type_name = self.program.type_name(&type_name);
                                    let field_name = self.program.field_name(&field_name);
                                    return Err(Error::Runtime {
                                        message: format!(
                                            "Unknown field {} for type {}",
                                            type_name, field_name
                                        ),
                                        line: loc.line,
                                        column: loc.col,
                                    });
                                }
                                needed_fields.remove(&field_name);
                                object.insert(field_name, val);
                            }
                            None => break,
                            Some(v) => {
                                self.stack.push(v);
                                break;
                            }
                        }
                    }

                    // Up until here query and create were the same. Now instead of creating a new object
                    // we are going to find an existing one where all the fields match.
                    let mut found = vec![];
                    for (i, (_tick, obj)) in self
                        .objects
                        .get(&type_name)
                        .unwrap_or(&vec![])
                        .iter()
                        .enumerate()
                    {
                        let mut matches = true;
                        for (field_name, val) in object.iter() {
                            if let Some(obj_val) = obj.get(field_name) {
                                if *obj_val != *val {
                                    matches = false;
                                    break;
                                }
                            } else {
                                matches = false;
                                break;
                            }
                        }
                        if matches {
                            found.push(i);
                        }
                    }

                    // Now we have multiple possible objects that match the query. For now just pick a random one.
                    if found.is_empty() {
                        return Err(Error::Runtime {
                            message: "No matching object found".to_owned(),
                            line: loc.line,
                            column: loc.col,
                        });
                    }

                    let i = rand::thread_rng().gen_range(0..found.len());
                    let obj_i = found[i];
                    self.stack.push(Value::Object(type_name, obj_i));
                }
                Op::Generate(typ) => {
                    // Generate values for the type.
                    // This is like the real magic of the entire project.
                    if typ == self.program.int {
                        let mut rng = rand::thread_rng();
                        let val = rng.gen_range(0..100);
                        self.stack.push(Value::Int(val));
                    } else if typ == self.program.dec {
                        let mut rng = rand::thread_rng();
                        let val = rng.gen_range(0.0..1.0);
                        self.stack.push(Value::Dec(val));
                    } else if typ == self.program.bool {
                        let mut rng = rand::thread_rng();
                        let val = rng.gen::<bool>();
                        self.stack.push(Value::Bool(val));
                    } else if typ == self.program.string {
                        let rng = rand::thread_rng();
                        let val: String = rng
                            .sample_iter(&Alphanumeric)
                            .take(30)
                            .map(char::from)
                            .collect();
                        let interned_val = self.program.intern_string(&val);
                        self.stack.push(Value::String(interned_val));
                    } else if self.program.types.contains_key(&typ) {
                        // Random object types.
                        // Theres some questions here, like do we generate a new value or do we pull a random value?
                        todo!()
                    } else {
                        return Err(Error::Runtime {
                            message: format!("Unknown Type {}", self.program.type_name(&typ)),
                            line: loc.line,
                            column: loc.col,
                        });
                    }
                }
                // Can probably clean this up a lot with macros.
                Op::Add => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Object(_, _), _) | (_, Value::Object(_, _)) => {
                            return Err(Error::Runtime {
                                message: "Can not add objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Bool(_), _) | (_, Value::Bool(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not add booleans.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::String(_), _) | (_, Value::String(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not add strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(a + b));
                        }
                        (Value::Int(a), Value::Dec(b)) => {
                            self.stack.push(Value::Dec(a as f64 + b));
                        }
                        (Value::Dec(a), Value::Int(b)) => {
                            self.stack.push(Value::Dec(a + b as f64));
                        }
                        (Value::Dec(a), Value::Dec(b)) => {
                            self.stack.push(Value::Dec(a + b));
                        }
                    }
                }
                Op::Sub => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Object(_, _), _) | (_, Value::Object(_, _)) => {
                            return Err(Error::Runtime {
                                message: "Can not subtract objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Bool(_), _) | (_, Value::Bool(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not subtract booleans.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::String(_), _) | (_, Value::String(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not subtract strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(a - b));
                        }
                        (Value::Int(a), Value::Dec(b)) => {
                            self.stack.push(Value::Dec(a as f64 - b));
                        }
                        (Value::Dec(a), Value::Int(b)) => {
                            self.stack.push(Value::Dec(a - b as f64));
                        }
                        (Value::Dec(a), Value::Dec(b)) => {
                            self.stack.push(Value::Dec(a - b));
                        }
                    }
                }
                Op::Mul => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Object(_, _), _) | (_, Value::Object(_, _)) => {
                            return Err(Error::Runtime {
                                message: "Can not multiply objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Bool(_), _) | (_, Value::Bool(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not multiply booleans.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::String(_), _) | (_, Value::String(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not multiply strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(a * b));
                        }
                        (Value::Int(a), Value::Dec(b)) => {
                            self.stack.push(Value::Dec(a as f64 * b));
                        }
                        (Value::Dec(a), Value::Int(b)) => {
                            self.stack.push(Value::Dec(a * b as f64));
                        }
                        (Value::Dec(a), Value::Dec(b)) => {
                            self.stack.push(Value::Dec(a * b));
                        }
                    }
                }
                Op::Div => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Object(_, _), _) | (_, Value::Object(_, _)) => {
                            return Err(Error::Runtime {
                                message: "Can not divide objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Bool(_), _) | (_, Value::Bool(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not divide booleans.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::String(_), _) | (_, Value::String(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not divide strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        // just panics still, todo wrap in a runtime error
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(a / b));
                        }
                        (Value::Int(a), Value::Dec(b)) => {
                            self.stack.push(Value::Dec(a as f64 / b));
                        }
                        (Value::Dec(a), Value::Int(b)) => {
                            self.stack.push(Value::Dec(a / b as f64));
                        }
                        (Value::Dec(a), Value::Dec(b)) => {
                            self.stack.push(Value::Dec(a / b));
                        }
                    }
                }
                Op::Mod => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Object(_, _), _) | (_, Value::Object(_, _)) => {
                            return Err(Error::Runtime {
                                message: "Can not mod objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Bool(_), _) | (_, Value::Bool(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not mod booleans.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::String(_), _) | (_, Value::String(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not mod strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Int(a % b));
                        }
                        (Value::Int(a), Value::Dec(b)) => {
                            self.stack.push(Value::Dec(a as f64 % b));
                        }
                        (Value::Dec(a), Value::Int(b)) => {
                            self.stack.push(Value::Dec(a % b as f64));
                        }
                        (Value::Dec(a), Value::Dec(b)) => {
                            self.stack.push(Value::Dec(a % b));
                        }
                    }
                }
                Op::Neg => {
                    let a = self.stack_pop()?;
                    match a {
                        Value::Nil => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        Value::Tag(_) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        Value::Object(_, _) => {
                            return Err(Error::Runtime {
                                message: "Can not negate object.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        Value::Bool(_) => {
                            return Err(Error::Runtime {
                                message: "Can not negate bool (try !).".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        Value::String(_) => {
                            return Err(Error::Runtime {
                                message: "Can not negate string.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        Value::Int(i) => {
                            self.stack.push(Value::Int(-i));
                        }
                        Value::Dec(f) => {
                            self.stack.push(Value::Dec(-f));
                        }
                    }
                }
                Op::Eq => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a == b));
                        }
                        (Value::Int(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool(a as f64 == b));
                        }
                        (Value::Dec(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a == b as f64));
                        }
                        (Value::Dec(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool(a == b));
                        }
                        (Value::Bool(a), Value::Bool(b)) => {
                            self.stack.push(Value::Bool(a == b));
                        }
                        (Value::String(a), Value::String(b)) => {
                            self.stack.push(Value::Bool(a == b));
                        }
                        (Value::Object(a_typ, a_i), Value::Object(b_typ, b_i)) => {
                            // if a_typ != b_typ {
                            //     self.stack.push(Value::Bool(false));
                            // } else {
                            //     if let Some(objs) = self.objects.get(&a_typ) {
                            //         let (_, a) = &objs[a_i];
                            //         let (_, b) = &objs[b_i];
                            //         // todo: The type check ensures these have the same keys so it's not really needed.
                            //         let same_keys =
                            //             a.len() == b.len() && a.keys().all(|k| b.contains_key(k));

                            //         // todo: Turn this into an AND of all the values being equal.
                            //         todo!()
                            //     }
                            // }
                            todo!()
                        }
                        (_, _) => self.stack.push(Value::Bool(false)),
                    }
                }
                Op::Neq => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a != b));
                        }
                        (Value::Int(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool(a as f64 != b));
                        }
                        (Value::Dec(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a != b as f64));
                        }
                        (Value::Dec(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool(a != b));
                        }
                        (Value::Bool(a), Value::Bool(b)) => {
                            self.stack.push(Value::Bool(a != b));
                        }
                        (Value::String(a), Value::String(b)) => {
                            self.stack.push(Value::Bool(a != b));
                        }
                        (Value::Object(a_typ, a_i), Value::Object(b_typ, b_i)) => {
                            // if a_typ != b_typ {
                            //     self.stack.push(Value::Bool(true));
                            // } else {
                            //     if let Some(objs) = self.objects.get(&a_typ) {
                            //         let (_, a) = &objs[a_i];
                            //         let (_, b) = &objs[b_i];
                            //         // todo: The type check ensures these have the same keys so it's not really needed.
                            //         let same_keys =
                            //             a.len() == b.len() && a.keys().all(|k| b.contains_key(k));

                            //         // todo: Turn this into an AND of all the values being equal.
                            //         todo!()
                            //         }
                            //     }
                            // }
                            todo!()
                        }
                        (_, _) => self.stack.push(Value::Bool(true)),
                    }
                }
                Op::Lt => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Object(_, _), _) | (_, Value::Object(_, _)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Bool(_), _) | (_, Value::Bool(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare booleans.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::String(_), _) | (_, Value::String(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a < b));
                        }
                        (Value::Int(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool((a as f64) < b));
                        }
                        (Value::Dec(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a < b as f64));
                        }
                        (Value::Dec(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool(a < b));
                        }
                    }
                }
                Op::Gt => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Object(_, _), _) | (_, Value::Object(_, _)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Bool(_), _) | (_, Value::Bool(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare booleans.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::String(_), _) | (_, Value::String(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a > b));
                        }
                        (Value::Int(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool((a as f64) > b));
                        }
                        (Value::Dec(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a > b as f64));
                        }
                        (Value::Dec(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool(a > b));
                        }
                    }
                }
                Op::Lte => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Object(_, _), _) | (_, Value::Object(_, _)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Bool(_), _) | (_, Value::Bool(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare booleans.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::String(_), _) | (_, Value::String(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a <= b));
                        }
                        (Value::Int(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool((a as f64) <= b));
                        }
                        (Value::Dec(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a <= b as f64));
                        }
                        (Value::Dec(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool(a <= b));
                        }
                    }
                }
                Op::Gte => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Object(_, _), _) | (_, Value::Object(_, _)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Bool(_), _) | (_, Value::Bool(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare booleans.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::String(_), _) | (_, Value::String(_)) => {
                            return Err(Error::Runtime {
                                message: "Can not compare strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a >= b));
                        }
                        (Value::Int(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool((a as f64) >= b));
                        }
                        (Value::Dec(a), Value::Int(b)) => {
                            self.stack.push(Value::Bool(a >= b as f64));
                        }
                        (Value::Dec(a), Value::Dec(b)) => {
                            self.stack.push(Value::Bool(a >= b));
                        }
                    }
                }
                Op::And => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(_), _)
                        | (_, Value::Int(_))
                        | (Value::Dec(_), _)
                        | (_, Value::Dec(_)) => {
                            return Err(Error::Runtime {
                                message: "Cannot AND numbers".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::String(_), _) | (_, Value::String(_)) => {
                            return Err(Error::Runtime {
                                message: "Cannot AND strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Object(_, _), _) | (_, Value::Object(_, _)) => {
                            return Err(Error::Runtime {
                                message: "Cannot AND objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Bool(a), Value::Bool(b)) => {
                            self.stack.push(Value::Bool(a && b));
                        }
                    }
                }
                Op::Or => {
                    let b = self.stack_pop()?;
                    let a = self.stack_pop()?;
                    match (a, b) {
                        (Value::Nil, _) | (_, Value::Nil) => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Tag(_), _) | (_, Value::Tag(_)) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Int(_), _)
                        | (_, Value::Int(_))
                        | (Value::Dec(_), _)
                        | (_, Value::Dec(_)) => {
                            return Err(Error::Runtime {
                                message: "Cannot OR numbers".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::String(_), _) | (_, Value::String(_)) => {
                            return Err(Error::Runtime {
                                message: "Cannot OR strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Object(_, _), _) | (_, Value::Object(_, _)) => {
                            return Err(Error::Runtime {
                                message: "Cannot OR objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        (Value::Bool(a), Value::Bool(b)) => {
                            self.stack.push(Value::Bool(a || b));
                        }
                    }
                }
                Op::Not => {
                    let a = self.stack_pop()?;
                    match a {
                        Value::Nil => {
                            return Err(Error::Runtime {
                                message: "Uninitialized Variable".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        Value::Tag(_) => {
                            return Err(Error::Runtime {
                                message:
                                    "Compiler error: Found tag on stack outside of object creation."
                                        .to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        Value::Int(_) | Value::Dec(_) => {
                            return Err(Error::Runtime {
                                message: "Cannot NOT numbers".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        Value::String(_) => {
                            return Err(Error::Runtime {
                                message: "Cannot NOT strings.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        Value::Object(_, _) => {
                            return Err(Error::Runtime {
                                message: "Cannot NOT objects.".to_owned(),
                                line: loc.line,
                                column: loc.col,
                            })
                        }
                        Value::Bool(a) => {
                            self.stack.push(Value::Bool(!a));
                        }
                    }
                }
            }
            self.ip += 1;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runs() {
        let program = r#"
        type Foo { id: Int };
        type Bar { id: Int, foo: Foo };
        #print 1;
        var x;
        var y = 1;
        x = 2;
        #print x;
        #print y;
        #print x + y;
        y = x;
        #print y;
        var foo = create Foo { id: x };
        print foo.id;
        foo.id = 999;
        print foo.id;
        #print foo;
        var b = create Bar { id: 1, foo: foo };
        print 12345;
        print b.foo.id;
        b.foo.id = 888;
        print foo.id;
        var foo_again = query Foo { id: x};
        var bar = create Bar { id: @Int, foo: foo_again };
        # var bar_again = query Bar { id: bar.id };
        #print foo;
        #print bar;
        #print @String;
        #print foo_again;
        #var dont_exist = query Foo { id: 99999};
        "#;
        let ast = crate::parser::parse_program(program).unwrap();
        let program = crate::compiler::compile(ast);
        // program.decompile();
        let mut vm = Vm::new(program);
        let results = vm.run();
        for error in results.errors {
            println!("{:?}", error);
        }
        for output in &results.output {
            println!("{}", output.value);
        }
    }

    #[test]
    fn test_poc() {
        let program = r#"
        type Organization {
            i: Int,
            name: String,
            number_of_employees: Int,
        }
        type Repository {
            i: Int,
            name: String,
            organization: Organization,
        }
        type Issue {
            i: Int,
            repository: Repository,
            name: String
        }
        
        steve_org = create Organization {
            i: 1,
            name: @String,
            number_of_employees: @Int,
        }
        
        # 3 random repositories
        repo1 = create Repository {
            i: @Int,
            name: @String,
            organization: steve_org
        }
        repo2 = create Repository {
            i: @Int,
            name: @String,
            organization: steve_org
        }
        repo3 = create Repository {
            i: @Int,
            name: @String,
            organization: steve_org
        }
        
        # 3 random issues 
        random_repo = query Repository{}
        issue1 = create Issue {
            i: @Int,
            name: @String,
            repository: random_repo
        }
        random_repo = query Repository{}
        issue2 = create Issue {
            i: @Int,
            name: @String,
            repository: random_repo
        }
        random_repo = query Repository{}
        issue3 = create Issue {
            i: @Int,
            name: @String,
            repository: random_repo
        }
        
        print 1 + 2 / 3 * 4 % -6 <= 999 && !(1==1);
        "#;
        let ast = crate::parser::parse_program(program).unwrap();
        let program = crate::compiler::compile(ast);
        // program.decompile();
        let mut vm = Vm::new(program);
        let results = vm.run();
        for error in results.errors {
            println!("{:?}", error);
        }
        for output in &results.output {
            println!("{}", output.value);
        }
    }
}
