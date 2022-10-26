use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Write};

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
    // there's no need for this to be a copy of the program
    // it could just be a reference, but that means adding
    // a lifetime which won't work through wasm-bindgen.
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
                let object_type = &object_type_name;
                let fields = &display_fields;
                // let mut temp_value = String::new();
                // write!(temp_value, "{} {{", object_type).unwrap();
                // for (name, value) in fields {
                //     write!(temp_value, " {}: {},", name, value).unwrap();
                // }
                // write!(temp_value, " }}").unwrap();
                DisplayValue::Object {
                    object_type: object_type_name,
                    fields: display_fields,
                    //value: temp_value,
                }
            }
        }
    }

    pub fn try_run(&mut self) -> Result<(), Error> {
        loop {
            self.tick.tick();
            let op = &self.program.code[self.ip];
            use crate::bytecode::Op;
            match op {
                Op::Halt => {
                    break;
                }
                Op::Return => (),
                Op::Constant(i) => {
                    let val = &self.program.constants[*i];
                    self.stack.push(val.clone());
                }
                Op::Print => {
                    let val = self.stack.pop().unwrap();
                    let display_val = self.display(&val);
                    self.output.push(TickOutput {
                        tick: self.tick,
                        value: format!("{}", display_val),
                    });
                }
                Op::Pop => {
                    _ = self.stack.pop().unwrap();
                }
                Op::DefineGlobal(var_name) => {
                    let val = self.stack.pop().unwrap();
                    self.globals.insert(*var_name, val);
                }
                // todo: assign to more than just variables, specifically fields
                Op::Assign(var_name) => {
                    let val = self.stack.pop().unwrap();
                    // todo: check if var is defined otherwise it's a runtime error
                    self.globals.insert(*var_name, val.clone());
                    // put value back on the stack
                    self.stack.push(val);
                }
                Op::Read(var_name) => {
                    let val = self.globals.get(var_name).unwrap();
                    self.stack.push(val.clone());
                }
                Op::Get(_) => todo!(),
                Op::Tag(field_name) => {
                    self.stack.push(Value::Tag(*field_name));
                }
                Op::Create(type_name) => {
                    if !self.program.types.contains_key(type_name) {
                        let type_name = self.program.type_name(type_name);
                        let loc = self.program.code_locs[self.ip].clone();
                        return Err(Error::Runtime {
                            message: format!("Type {} not defined", type_name),
                            line: loc.line,
                            column: loc.col,
                        });
                    }
                    let typedef = self.program.types.get(type_name).unwrap();
                    let mut needed_fields: HashSet<FieldName> =
                        typedef.fields.keys().cloned().collect();
                    let mut object = HashMap::new();
                    // collect fields from the stack
                    loop {
                        let val = self.stack.pop();
                        match val {
                            Some(Value::Tag(field_name)) => {
                                let val = self.stack.pop().unwrap();
                                if !needed_fields.contains(&field_name) {
                                    let type_name = self.program.type_name(type_name);
                                    let loc = self.program.code_locs[self.ip].clone();
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
                        let type_name = self.program.type_name(type_name);
                        let loc = self.program.code_locs[self.ip].clone();
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
                    let objs = self.objects.entry(*type_name).or_default();
                    objs.push((self.tick, object));
                    // Put the object on the stack
                    self.stack.push(Value::Object(*type_name, objs.len() - 1));
                }
                Op::Generate(_) => todo!(),
                Op::Add => todo!(),
                Op::Sub => todo!(),
                Op::Mul => todo!(),
                Op::Div => todo!(),
                Op::Mod => todo!(),
                Op::Eq => todo!(),
                Op::Neq => todo!(),
                Op::Lt => todo!(),
                Op::Gt => todo!(),
                Op::Lte => todo!(),
                Op::Gte => todo!(),
                Op::And => todo!(),
                Op::Or => todo!(),
                Op::Not => todo!(),
                Op::Neg => todo!(),
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
        print 1;
        var x;
        var y = 1;
        x = 2;
        print x;
        print y;
        y = x;
        print y;
        var foo = create Foo { id: x };
        print foo;
        var b = create Bar { id: 1, foo: foo };
        print b;
        #var foo_again = query Foo { id: x};
        #var bar = create Bar { id: @, foo: foo_again };
        # var bar_again = query Bar { id: bar.id };
        #print foo;
        #print bar;
        "#;
        let ast = crate::parser::parse_program(program).unwrap();
        let program = crate::compiler::compile(ast);
        // program.decompile();
        let mut vm = Vm::new(&program);
        let results = vm.run();
        for (_tick, line) in &results.output {
            println!("{}", line);
        }
    }
}
