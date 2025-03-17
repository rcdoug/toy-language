use std::{collections::HashMap, process::Output};

/// A simple stack machine with I/O buffers for interacting with the UI.
pub struct StackMachine {
    stack: Vec<i32>,
    memory: HashMap<i32, i32>,
    labels: HashMap<String, usize>,
    program_counter: usize,
    program: Vec<Instruction>,
    /// Accumulated output messages from the machine.
    pub output: Vec<String>,
    /// When set, indicates the machine is waiting for user input.
    pub input_request: Option<String>,
    /// Configure output mode with respect to the print instruction
    pub output_mode: OutputMode,
}

/// The Instruction enum defines the supported operations.
#[derive(Clone, Debug)]
pub enum Instruction {
    // Stack Operations
    Push(i32), Pop, Dup, Swap, Rot, Clear,

    // Arithmetic
    Add, Sub, Mul, Div, Rem, Neg, Inc, Dec,

    // Comparison
    Eq, Ne, Le, Ge, Lt, Gt,

    // Control Flow
    Call(String), Ret, Retv(i32), Jump(String), Brt(String), Brz(String), Label(String), Halt,

    // Memory
    Load(i32), Store(i32), Alloc(i32), Free(i32),

    // I/O
    /// Instead of printing directly, the machine will capture this output.
    Print,
    /// Instead of reading immediately, the machine will request input from the UI.
    Read,
    /// Append a message to the output.
    Write(String),
    /// For demonstration, similar to Read.
    Scan,

    // Bitwise/Logical
    And, Or, Xor, Not, Shl, Shr, Bool,

    // Debugging
    Dump, Trace(bool),
}

pub enum OutputMode {
    Raw,
    Vector,
}

impl StackMachine {
    pub fn new() -> Self {
        StackMachine {
            stack: Vec::new(),
            memory: HashMap::new(),
            labels: HashMap::new(),
            program_counter: 0,
            program: Vec::new(),
            output: Vec::new(),
            input_request: None,
            output_mode: OutputMode::Vector,
        }
    }

    /// Loads the program and precomputes label positions.
    pub fn load_program(&mut self, program: Vec<Instruction>) {
        self.program = program;
        self.index_labels();
    }

    fn index_labels(&mut self) {
        for (index, instruction) in self.program.iter().enumerate() {
            if let Instruction::Label(name) = instruction {
                self.labels.insert(name.clone(), index);
            }
        }
    }

    /// Steps through one instruction, if available.
    pub fn step(&mut self) {
        if self.program_counter < self.program.len() {
            let instruction = self.program[self.program_counter].clone();
            self.program_counter += 1;
            self.perform_operation(instruction);
        }
    }

    /// Executes instructions until the end of the program.
    pub fn execute(&mut self) {
        while self.program_counter < self.program.len() {
            self.step();
        }
    }

    // Getter methods for the UI to inspect the machine's state.
    pub fn get_program_counter(&self) -> usize {
        self.program_counter
    }

    pub fn get_program(&self) -> &Vec<Instruction> {
        &self.program
    }

    pub fn get_stack(&self) -> &Vec<i32> {
        &self.stack
    }

    pub fn push_value(&mut self, value: i32) {
        self.stack.push(value);
    }
    
    /// Handles the execution of a single instruction.
    fn perform_operation(&mut self, instruction: Instruction) {
        match instruction {
            // Stack Operations
            Instruction::Push(value) => self.stack.push(value),
            Instruction::Pop => {
                self.stack.pop();
            }
            Instruction::Dup => {
                if let Some(&top) = self.stack.last() {
                    self.stack.push(top);
                }
            }
            Instruction::Swap => {
                if self.stack.len() >= 2 {
                    let len = self.stack.len();
                    self.stack.swap(len - 1, len - 2);
                }
            }
            Instruction::Rot => {
                if self.stack.len() >= 3 {
                    let a = self.stack.pop().expect("Stack underflow");
                    let b = self.stack.pop().expect("Stack underflow");
                    let c = self.stack.pop().expect("Stack underflow");
                    self.stack.push(a);
                    self.stack.push(c);
                    self.stack.push(b);
                }
            }
            Instruction::Clear => self.stack.clear(),

            // Arithmetic
            Instruction::Add => self.binary_op(|a, b| a + b),
            Instruction::Sub => self.binary_op(|a, b| a - b),
            Instruction::Mul => self.binary_op(|a, b| a * b),
            Instruction::Div => self.binary_op(|a, b| a / b),
            Instruction::Rem => self.binary_op(|a, b| a % b),
            Instruction::Neg => self.unary_op(|a| -a),
            Instruction::Inc => self.unary_op(|a| a + 1),
            Instruction::Dec => self.unary_op(|a| a - 1),

            // Comparison
            Instruction::Eq => self.binary_op(|a, b| if a == b { 1 } else { 0 }),
            Instruction::Ne => self.binary_op(|a, b| if a != b { 1 } else { 0 }),
            Instruction::Le => self.binary_op(|a, b| if a <= b { 1 } else { 0 }),
            Instruction::Ge => self.binary_op(|a, b| if a >= b { 1 } else { 0 }),
            Instruction::Lt => self.binary_op(|a, b| if a < b { 1 } else { 0 }),
            Instruction::Gt => self.binary_op(|a, b| if a > b { 1 } else { 0 }),

            // Control Flow
            Instruction::Call(label) => {
                // Placeholder: you could implement a call stack here.
                self.output
                    .push(format!("Call '{}' not implemented.", label));
            }
            Instruction::Ret => {
                self.output.push("Ret not implemented.".to_string());
            }
            Instruction::Retv(value) => {
                self.output
                    .push(format!("Retv({}) not implemented.", value));
            }
            Instruction::Jump(label) => {
                if let Some(&pos) = self.labels.get(&label) {
                    self.program_counter = pos;
                }
            }
            Instruction::Brt(label) => {
                if self.stack.pop().unwrap_or(0) != 0 {
                    if let Some(&pos) = self.labels.get(&label) {
                        self.program_counter = pos;
                    }
                }
            }
            Instruction::Brz(label) => {
                if self.stack.pop().unwrap_or(0) == 0 {
                    if let Some(&pos) = self.labels.get(&label) {
                        self.program_counter = pos;
                    }
                }
            }
            Instruction::Label(_) => {
                // Labels serve only for jump targets and are no-ops at runtime.
            }
            Instruction::Halt => {
                self.program_counter = self.program.len();
            }

            // Memory
            Instruction::Load(address) => {
                let value = self.memory.get(&address).copied().unwrap_or(0);
                self.stack.push(value);
            }
            Instruction::Store(address) => {
                if let Some(value) = self.stack.pop() {
                    self.memory.insert(address, value);
                }
            }
            Instruction::Alloc(size) => {
                for i in 0..size {
                    self.memory.insert(i, 0);
                }
            }
            Instruction::Free(address) => {
                self.memory.remove(&address);
            }

            // I/O
            Instruction::Print => {
                match self.output_mode {
                    OutputMode::Raw => {
                        if let Some(&top) = self.stack.last() {
                            println!("{}", top);
                        } else {
                            println!("Stack is empty");
                        }
                    }
                    OutputMode::Vector => {
                        if let Some(&top) = self.stack.last() {
                            self.output.push(format!("{}", top));
                        } else {
                            self.output.push("Stack is empty".to_string());
                        }
                    }
                }
            }
            Instruction::Read => {
                // Instead of blocking for input, signal to the UI.
                self.input_request = Some("Enter a number:".to_string());
            }
            Instruction::Write(message) => {
                self.output.push(message);
            }
            Instruction::Scan => {
                // For demonstration purposes, behave like Read.
                self.input_request = Some("Scan input:".to_string());
            }

            // Bitwise/Logical
            Instruction::And => self.binary_op(|a, b| a & b),
            Instruction::Or => self.binary_op(|a, b| a | b),
            Instruction::Xor => self.binary_op(|a, b| a ^ b),
            Instruction::Not => self.unary_op(|a| !a),
            Instruction::Shl => self.binary_op(|a, b| a << b),
            Instruction::Shr => self.binary_op(|a, b| a >> b),
            Instruction::Bool => self.unary_op(|a| if a != 0 { 1 } else { 0 }),

            // Debugging
            Instruction::Dump => {
                self.output.push(format!("Stack: {:?}", self.stack));
                self.output.push(format!("Memory: {:?}", self.memory));
            }
            Instruction::Trace(on) => {
                if on {
                    self.output.push("Tracing enabled".to_string());
                } else {
                    self.output.push("Tracing disabled".to_string());
                }
            }
        }
    }

    fn binary_op<F>(&mut self, op: F)
    where
        F: Fn(i32, i32) -> i32,
    {
        let b = self.stack.pop().expect("Stack underflow");
        let a = self.stack.pop().expect("Stack underflow");
        self.stack.push(op(a, b));
    }

    fn unary_op<F>(&mut self, op: F)
    where
        F: Fn(i32) -> i32,
    {
        let a = self.stack.pop().expect("Stack underflow");
        self.stack.push(op(a));
    }
}