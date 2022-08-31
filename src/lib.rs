use log::{LevelFilter};
use simplelog::{Config, SimpleLogger};
use crate::generator::generate;

mod frontend;
mod generator;

pub fn compile_and_run(program: &str) -> Result<i32, String> {

    let _ = SimpleLogger::init(LevelFilter::Debug, Config::default());

    let script = frontend::parser::script(program).map_err(|e| e.to_string())?;

    generate(script);

    Ok(1)
}


