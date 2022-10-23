use serde::Serialize;
use thiserror::Error;

#[derive(Error, Debug, Serialize)]
#[serde(tag = "type")]
pub enum Error {
    #[error("Parse Error")]
    Parse {
        line: u32,
        column: u32,
        message: String,
    },
    #[error("Runtime Error")]
    Runtime {
        line: u32,
        column: u32,
        message: String,
    },
}
