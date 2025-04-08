mod lexer;
mod ast;
mod parser;
mod codegen;
mod stack_machine;

use lexer::Lexer;
use parser::Parser;
use codegen::CodeGenerator;
use std::fs;

fn main() {
    // ! MODIFY FILE PATH TO TEST DIFFERENT PROGRAMS
    let source = fs::read_to_string("sample_programs/gcd.txt").expect("Unable to read file");

    // lexer.rs
    let mut lex = Lexer::new(&source);
    let mut tokens = Vec::new();
    while let Some(tok) = lex.next_token() {
        tokens.push(tok);
    }
    println!("Tokens: {:?}", tokens);
    println!("\n -------------------------- \n");

    // ast.rs
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program();
    println!("AST: {:#?}", program);
    println!("\n -------------------------- \n");

    // codegen.rs
    let mut codegen = CodeGenerator::new();
    let machine_code = codegen.generate(program);
    println!("Generated Stack Machine Code:");
    for instr in &machine_code {
        println!("{:?}", instr);
    }
    println!("\n -------------------------- \n");

    // stack_machine.rs
    let mut sm = stack_machine::StackMachine::new();
    sm.load_program(machine_code);
    sm.execute();
}
