use crate::parser::ASTNode;
use crate::tokenizer::Token;
use crate::stack_machine::Instruction;

pub struct CodeGenerator {
    instructions: Vec<Instruction>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }

    pub fn generate(&mut self, node: &ASTNode) {
        match node {
            ASTNode::Num(value) => self.instructions.push(Instruction::Push(*value)),
            ASTNode::BinOp { left, op, right } => {
                self.generate(left);
                self.generate(right);
                match op {
                    Token::Plus => self.instructions.push(Instruction::Add),
                    Token::Minus => self.instructions.push(Instruction::Sub),
                    Token::Mul => self.instructions.push(Instruction::Mul),
                    Token::Div => self.instructions.push(Instruction::Div),
                    _ => panic!("Unexpected operator"),
                }
            }
        }
    }

    pub fn get_code(&mut self) -> Vec<Instruction> {
        self.instructions.push(Instruction::Print);
        self.instructions.clone()
    }
}
