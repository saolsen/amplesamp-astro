mod utils;

use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet() {
    alert("Hello, amplesamp-wasm!");
}

// #[wasm_bindgen]
// pub struct Error {
//     kind: String,
//     line: u32,
//     column: u32,
//     message: String,
// }

// #[wasm_bindgen]
// impl Error {
//     #[wasm_bindgen(constructor)]
//     pub fn new(kind: String, line: u32, column: u32, message: String) -> Error {
//         Error {
//             kind,
//             line,
//             column,
//             message,
//         }
//     }
// }

// use amplesamp_lang::Error;

#[wasm_bindgen]
pub fn compile(source: String) -> JsValue {
    let ast = amplesamp_lang::parse_program(&source);
    let errors = match ast {
        Ok(ast) => {
            let _program = amplesamp_lang::compile(ast);
            vec![]
        }
        Err(errs) => errs,
    };
    serde_wasm_bindgen::to_value(&errors).unwrap()
}
