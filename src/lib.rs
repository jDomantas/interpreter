// TODO: don't forget to remove after developing
#![allow(dead_code)]

mod position;
mod parsing {
    mod tokens;
    mod lexer;
    mod parser;
}
mod ast;

#[cfg(test)]
mod tests { }
