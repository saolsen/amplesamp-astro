mod ast;
mod bytecode;
mod compiler;
mod error;
mod parser;
mod vm;

pub use bytecode::Program;
pub use compiler::compile;
pub use parser::parse_program;
pub use vm::Results;
pub use vm::Tick;
pub use vm::Vm;

pub use error::Error;

//pub fn compile(input: &str) -> Result<(), Vec<error::Error>> {}

// pub fn run<'a>(input: &str) -> Results<'a> {
//     let ast = parser::parse_program(input);
//     match ast {
//         Ok(ast) => {
//             let program = compiler::compile(ast);
//             let mut vm = Vm::new(&program);
//             let results = vm.run();
//             results
//         }
//         Err(errs) => {
//             let mut results = Results::default();
//             for e in errs {
//                 results.errors.push((Tick::new(), e));
//             }
//             results
//         }
//     }
// }
