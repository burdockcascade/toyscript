use crate::frontend::Token;
use log::{debug, error, trace};

pub fn generate(script: Vec<Token>) {

    for statement in script {

        match statement {

            Token::Function(name, params, statements) => compile_function(name, params, statements),

            _ => error!("illegal statement")

        }

    }

}

fn compile_function(name: String, params: Vec<String>, statements: Vec<Token>) {
    debug!("new function '{}' discovered with {} parameters", name, params.len());
}
