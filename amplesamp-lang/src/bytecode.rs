use core::fmt::Debug;
use std::{collections::HashMap, fmt::Formatter, hash::Hash};

use serde::Serialize;
use string_interner::{symbol::SymbolU32, StringInterner, Symbol};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Nil,
    Int(i64),
    Dec(f64),
    Bool(bool),
    String(InternedString),
    Tag(FieldName),
    Object(TypeName, usize),
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Halt,
    Return,
    Pop,
    Print,
    Constant(usize),
    Get(FieldName),
    Tag(FieldName), // give a name to the thing on the top of the stack, so we can build objects later
    Create(TypeName),
    Query(TypeName),
    DefineGlobal(VariableName),
    AssignVar(VariableName),
    AssignField(FieldName),
    Read(VariableName),
    Generate(TypeName), // todo: will need constraints somewhere eventuially
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
    Not,
    Neg,
}

#[derive(Debug, Clone, Serialize)]
pub struct Loc {
    pub len: usize,
    pub line: u32,
    pub col: u32,
}

#[derive(Eq, Hash, PartialEq, Clone, Copy)]
pub struct InternedString(pub SymbolU32);
impl Debug for InternedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "s_{}", self.0.to_usize())
    }
}

#[derive(Eq, Hash, PartialEq, Clone, Copy)]
pub struct TypeName(pub SymbolU32);
impl Debug for TypeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "t_{}", self.0.to_usize())
    }
}
#[derive(Eq, Hash, PartialEq, Clone, Copy)]
pub struct FieldName(pub SymbolU32);
impl Debug for FieldName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "f_{}", self.0.to_usize())
    }
}
#[derive(Eq, Hash, PartialEq, Clone, Copy)]
pub struct VariableName(pub SymbolU32);
impl Debug for VariableName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "v_{}", self.0.to_usize())
    }
}

#[derive(Debug)]
pub struct Type {
    pub name: TypeName,
    pub fields: HashMap<FieldName, TypeName>,
    pub field_order: Vec<FieldName>,
}

#[derive(Debug)]
pub struct Program {
    strings: StringInterner,
    pub type_order: Vec<TypeName>,
    pub types: HashMap<TypeName, Type>,
    pub type_locs: HashMap<TypeName, Loc>,
    pub constants: Vec<Value>,
    // One big block of code for now, todo: functions
    pub code_locs: Vec<Loc>,
    pub code: Vec<Op>,

    pub int: TypeName,
    pub dec: TypeName,
    pub bool: TypeName,
    pub string: TypeName,
}

impl Program {
    pub fn new() -> Self {
        let mut strings = StringInterner::default();

        let int = TypeName(strings.get_or_intern("Int"));
        let dec = TypeName(strings.get_or_intern("Dec"));
        let bool = TypeName(strings.get_or_intern("Bool"));
        let string = TypeName(strings.get_or_intern("String"));

        Self {
            strings,
            type_order: vec![],
            types: HashMap::new(),
            type_locs: HashMap::new(),
            constants: Vec::new(),
            code_locs: Vec::new(),
            code: Vec::new(),
            int,
            dec,
            bool,
            string,
        }
    }

    /// Helpers for compiler to build programs

    pub fn intern_type_name(&mut self, name: &str) -> TypeName {
        TypeName(self.strings.get_or_intern(name))
    }

    pub fn intern_field_name(&mut self, name: &str) -> FieldName {
        FieldName(self.strings.get_or_intern(name))
    }

    pub fn intern_variable_name(&mut self, name: &str) -> VariableName {
        VariableName(self.strings.get_or_intern(name))
    }

    pub fn intern_string(&mut self, string: &str) -> InternedString {
        InternedString(self.strings.get_or_intern(string))
    }

    /// Helpers for vm to run programs

    pub fn type_name(&self, type_name: &TypeName) -> &str {
        self.strings.resolve(type_name.0).unwrap()
    }

    pub fn field_name(&self, field_name: &FieldName) -> &str {
        self.strings.resolve(field_name.0).unwrap()
    }

    pub fn variable_name(&self, variable_name: &VariableName) -> &str {
        self.strings.resolve(variable_name.0).unwrap()
    }

    pub fn string(&self, intern_string: &InternedString) -> &str {
        self.strings.resolve(intern_string.0).unwrap()
    }

    pub fn decompile(&self) {
        eprintln!("=== Types ===");
        for (name, typ) in &self.types {
            eprintln!("{} {{", self.type_name(name));
            for (field_n, field_t) in typ.fields.iter() {
                eprintln!(
                    "  {}: {}",
                    self.field_name(field_n),
                    self.type_name(field_t)
                );
            }
            eprintln!("}}");
        }

        eprintln!("=== Code ===");
        for (i, op) in self.code.iter().enumerate() {
            let loc = &self.code_locs[i];
            match op {
                Op::Return => eprintln!("{}:{}:\treturn", loc.line, loc.col),
                Op::Pop => eprintln!("{}:{}:\tpop", loc.line, loc.col),
                Op::Print => eprintln!("{}:{}:\tprint", loc.line, loc.col),
                Op::Constant(idx) => {
                    let val = &self.constants[*idx];
                    eprintln!("{}:{}:\tconstant {:?}", loc.line, loc.col, val)
                }
                Op::DefineGlobal(var_name) => {
                    eprintln!(
                        "{}:{}:\tdefine_global {}",
                        loc.line,
                        loc.col,
                        self.variable_name(var_name)
                    )
                }
                Op::Halt => {
                    eprintln!("{}:{}:\thalt", loc.line, loc.col,)
                }
                Op::AssignVar(var_name) => {
                    eprintln!(
                        "{}:{}:\tassign_var {}",
                        loc.line,
                        loc.col,
                        self.variable_name(var_name)
                    )
                }
                Op::AssignField(field_name) => {
                    eprintln!(
                        "{}:{}:\tassign_field {}",
                        loc.line,
                        loc.col,
                        self.field_name(field_name)
                    )
                }
                Op::Read(var_name) => {
                    eprintln!(
                        "{}:{}:\tread {}",
                        loc.line,
                        loc.col,
                        self.variable_name(var_name)
                    )
                }
                Op::Get(name) => {
                    eprintln!("{}:{}:\tget {}", loc.line, loc.col, self.field_name(name))
                }
                Op::Tag(name) => {
                    eprintln!("{}:{}:\ttag {}", loc.line, loc.col, self.field_name(name))
                }
                Op::Create(name) => {
                    eprintln!("{}:{}:\tcreate {}", loc.line, loc.col, self.type_name(name))
                }
                Op::Query(name) => {
                    eprintln!("{}:{}:\tquery {}", loc.line, loc.col, self.type_name(name))
                }
                Op::Generate(name) => {
                    eprintln!(
                        "{}:{}:\tgenerate {}",
                        loc.line,
                        loc.col,
                        self.type_name(name)
                    )
                }
                Op::Add => eprintln!("{}:{}:\tadd", loc.line, loc.col),
                Op::Sub => eprintln!("{}:{}:\tsub", loc.line, loc.col),
                Op::Mul => eprintln!("{}:{}:\tmul", loc.line, loc.col),
                Op::Div => eprintln!("{}:{}:\tdiv", loc.line, loc.col),
                Op::Mod => eprintln!("{}:{}:\tmod", loc.line, loc.col),
                Op::Eq => eprintln!("{}:{}:\teq", loc.line, loc.col),
                Op::Neq => eprintln!("{}:{}:\tneq", loc.line, loc.col),
                Op::Lt => eprintln!("{}:{}:\tlt", loc.line, loc.col),
                Op::Gt => eprintln!("{}:{}:\tgt", loc.line, loc.col),
                Op::Lte => eprintln!("{}:{}:\tlte", loc.line, loc.col),
                Op::Gte => eprintln!("{}:{}:\tgte", loc.line, loc.col),
                Op::And => eprintln!("{}:{}:\tand", loc.line, loc.col),
                Op::Or => eprintln!("{}:{}:\tor", loc.line, loc.col),
                Op::Not => eprintln!("{}:{}:\tnot", loc.line, loc.col),
                Op::Neg => eprintln!("{}:{}:\tneg", loc.line, loc.col),
            }
        }
    }
}
