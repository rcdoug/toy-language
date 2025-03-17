mod stack_machine;
mod tokenizer;
mod parser;
mod code_generator;

use std::fs;
use std::error::Error;
use tokenizer::{Lexer, Token};
use parser::Parser;
use code_generator::CodeGenerator;
use stack_machine::{OutputMode, StackMachine};

fn main() -> Result<(), Box<dyn Error>> {
    let input: String = fs::read_to_string("input.txt")?;

    // Tokenization
    let mut lexer = Lexer::new(&input);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        if token == Token::EOF {
            break;
        }
        tokens.push(token);
    }

    // Parsing
    let mut parser = Parser::new(tokens);
    let ast = parser.parse();

    // Code Generation
    let mut generator = CodeGenerator::new();
    generator.generate(&ast);
    let stack_code = generator.get_code();

    // Print stack machine code
    for instr in &stack_code {
        println!("{:?}", instr);
    }

    // Stack Machine Execution
    let mut sm = StackMachine::new();
    sm.output_mode = OutputMode::Raw;
    sm.load_program(stack_code);
    sm.execute();

    Ok(())
}
