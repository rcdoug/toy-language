use std::collections::HashMap;
use std::io::{self, Write};

pub struct StackMachine {
    stack: Vec<i32>,
    call_stack: CallStack,
    labels: HashMap<String, usize>,
    program_counter: usize,
    program: Vec<Instruction>,
    memory: Vec<i32>,
    gpr: i32,
}

struct StackFrame {
    pub ip: usize,
}

type CallStack = Vec<StackFrame>;

#[allow(non_camel_case_types, dead_code)]
#[derive(Clone, Debug)]
pub enum Instruction {
    // Basic Stack Operations  
    PUSH(i32),
    POP,
    DUP,
    SWAP,
    ROT,
    // Basic Arithmetic Operations
    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    // Comparison Operations
    EQ,
    NE,
    LT,
    GT,
    LE,
    GE,
    // Control Flow
    JUMP(String),
    CALL(String, usize),
    RET,
    RETV,
    BRT(String),
    BRZ(String),
    LABEL(String),
    // Memory Operations
    LOAD,
    STORE,
    SAVE,
    READ,
    // Additional Commands
    PRINT(Option<String>),
    END,
}

impl StackMachine {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            call_stack: CallStack::new(),
            labels: HashMap::new(),
            program_counter: 0,
            program: Vec::new(),
            memory: vec![0; 4096],
            gpr: 0,
        }
    }

    // Loads the program and registers labels by scanning for LABEL instructions.
    pub fn load_program(&mut self, program: Vec<Instruction>) {
        self.program = program;
        for (index, instruction) in self.program.iter().enumerate() {
            if let Instruction::LABEL(ref label_name) = instruction {
                if self.labels.contains_key(label_name) {
                    panic!("Duplicate label found: {}", label_name);
                }
                self.labels.insert(label_name.clone(), index);
            }
        }
    }

    pub fn execute(&mut self) {
        while self.program_counter < self.program.len() {
            let instruction = self.program[self.program_counter].clone();
            match instruction {
                // Basic Stack Operations
                Instruction::PUSH(value) => {
                    self.stack.push(value);
                    self.gpr = value;
                }
                Instruction::POP => {
                    let popped = self.stack.pop().expect("Stack underflow");
                    self.gpr = popped;
                }
                Instruction::DUP => {
                    if let Some(&top) = self.stack.last() {
                        self.stack.push(top);
                        self.gpr = top;
                    } else {
                        panic!("Stack underflow");
                    }
                }
                Instruction::SWAP => {
                    let a = self.stack.pop().expect("Stack underflow");
                    let b = self.stack.pop().expect("Stack underflow");
                    self.stack.push(a);
                    self.stack.push(b);
                }
                Instruction::ROT => {
                    let a = self.stack.pop().expect("Stack underflow");
                    let b = self.stack.pop().expect("Stack underflow");
                    let c = self.stack.pop().expect("Stack underflow");
                    self.stack.push(b);
                    self.stack.push(a);
                    self.stack.push(c);
                }

                // Arithmetic Operations
                Instruction::ADD => self.binary_op(|a, b| a + b),
                Instruction::SUB => self.binary_op(|a, b| a - b),
                Instruction::MUL => self.binary_op(|a, b| a * b),
                Instruction::DIV => self.binary_op(|a, b| {
                    if b == 0 {
                        panic!("Division by zero");
                    }
                    a / b
                }),
                Instruction::REM => self.binary_op(|a, b| a % b),

                // Comparison Operations
                Instruction::EQ => self.binary_op(|a, b| if a == b { 1 } else { 0 }),
                Instruction::NE => self.binary_op(|a, b| if a != b { 1 } else { 0 }),
                Instruction::LE => self.binary_op(|a, b| if a <= b { 1 } else { 0 }),
                Instruction::GE => self.binary_op(|a, b| if a >= b { 1 } else { 0 }),
                Instruction::LT => self.binary_op(|a, b| if a < b { 1 } else { 0 }),
                Instruction::GT => self.binary_op(|a, b| if a > b { 1 } else { 0 }),

                // Control Flow
                Instruction::JUMP(ref label) => {
                    if let Some(&target) = self.labels.get(label) {
                        self.program_counter = target;
                        continue;
                    } else {
                        panic!("Label not found: {}", label);
                    }
                }
                Instruction::CALL(ref label, _arg_count) => {
                    // Store return address
                    self.call_stack.push(StackFrame {
                        ip: self.program_counter + 1,
                    });
                    // Jump to function
                    if let Some(&target) = self.labels.get(label) {
                        self.program_counter = target;
                        continue; // Continue execution at the function label
                    } else {
                        panic!("Label not found: {}", label);
                    }
                }
                Instruction::RET => {
                    if let Some(frame) = self.call_stack.pop() {
                        self.program_counter = frame.ip;
                        continue;
                    } else {
                        panic!("Call stack underflow");
                    }
                }
                Instruction::RETV => {
                    let ret_value = self.stack.pop().expect("Stack underflow during RETV");
                    if let Some(frame) = self.call_stack.pop() {
                        self.stack.push(ret_value); // Push return value back
                        self.gpr = ret_value;
                        self.program_counter = frame.ip;
                        continue;
                    } else {
                        panic!("Call stack underflow");
                    }
                }
                Instruction::BRT(ref label) => {
                    let condition = self.stack.pop().expect("Stack underflow");
                    if condition != 0 {
                        if let Some(&target) = self.labels.get(label) {
                            self.program_counter = target;
                            continue;
                        } else {
                            panic!("Label not found: {}", label);
                        }
                    }
                }
                Instruction::BRZ(ref label) => {
                    let condition = self.stack.pop().expect("Stack underflow");
                    if condition == 0 {
                        if let Some(&target) = self.labels.get(label) {
                            self.program_counter = target;
                            continue;
                        } else {
                            panic!("Label not found: {}", label);
                        }
                    }
                }
                Instruction::LABEL(_label) => {
                    // No operation
                }

                // Memory Operations
                Instruction::LOAD => {
                    let addr = self.stack.pop().expect("Stack underflow") as usize;
                    if addr >= self.memory.len() {
                        panic!("Memory access out of bounds");
                    }
                    let value = self.memory[addr];
                    self.stack.push(value);
                    self.gpr = value;
                }
                Instruction::SAVE => {
                    let addr = self.stack.pop().expect("Stack underflow") as usize;
                    let value = self.stack.pop().expect("Stack underflow");
                    if addr >= self.memory.len() {
                        panic!("Memory access out of bounds");
                    }
                    self.memory[addr] = value;
                    self.stack.push(value);
                    self.gpr = value;
                }
                Instruction::STORE => {
                    let addr = self.stack.pop().expect("Stack underflow") as usize;
                    let value = self.stack.pop().expect("Stack underflow");
                    if addr >= self.memory.len() {
                        panic!("Memory access out of bounds");
                    }
                    self.memory[addr] = value;
                    self.gpr = value;
                }

                // I/O Operations
                Instruction::READ => {
                    let mut input = String::new();
                    print!("Input: ");
                    io::stdout().flush().unwrap();
                    io::stdin().read_line(&mut input).expect("Failed to read line");
                    let value = input.trim().parse::<i32>().expect("Invalid input");
                    self.stack.push(value);
                    self.gpr = value;
                }
                Instruction::PRINT(opt_str) => {
                    if let Some(literal) = opt_str {
                        println!("{}", literal);
                    } else {
                        let value = self.stack.pop().expect("Stack underflow");
                        println!("{}", value);
                        self.gpr = value;
                    }
                    io::stdout().flush().unwrap();
                }
                Instruction::END => {
                    break;
                }
            }
            self.program_counter += 1;
        }
    }

    fn binary_op<F>(&mut self, op: F)
    where
        F: Fn(i32, i32) -> i32,
    {
        let b = self.stack.pop().expect("Stack underflow");
        let a = self.stack.pop().expect("Stack underflow");
        let result = op(a, b);
        self.stack.push(result);
        self.gpr = result;
    }
}
