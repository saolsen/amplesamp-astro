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

#[wasm_bindgen]
pub fn compile(source: String) -> Result<(), JsValue> {
    let ast = amplesamp_lang::parse_program(&source);
    let errors = match ast {
        Ok(ast) => {
            let _program = amplesamp_lang::compile(ast);
            vec![]
        }
        Err(errs) => errs,
    };
    if errors.is_empty() {
        Ok(())
    } else {
        Err(serde_wasm_bindgen::to_value(&errors).unwrap())
    }
}

#[wasm_bindgen]
pub struct Vm {
    vm: amplesamp_lang::Vm,
}

#[wasm_bindgen]
impl Vm {
    #[wasm_bindgen(constructor)]
    pub fn new(source: String) -> Result<Vm, JsValue> {
        let ast = amplesamp_lang::parse_program(&source);
        match ast {
            Ok(ast) => {
                let program = amplesamp_lang::compile(ast);
                let vm = amplesamp_lang::Vm::new(program);
                Ok(Vm { vm })
            }
            Err(errors) => Err(serde_wasm_bindgen::to_value(&errors).unwrap()),
        }
    }

    #[wasm_bindgen]
    pub fn run(&mut self) -> JsValue {
        let results = self.vm.run();
        serde_wasm_bindgen::to_value(&results).unwrap()
    }
}
