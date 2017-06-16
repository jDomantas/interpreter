// TODO: don't forget to remove after developing
#![allow(dead_code)]

pub mod position;
pub mod errors;
pub mod parsing;
mod ast;
mod compiler;

#[cfg(test)]
mod tests { }
