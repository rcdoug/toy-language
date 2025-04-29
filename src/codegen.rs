use crate::ast::*;
use crate::stack_machine::Instruction;
use std::collections::HashMap;

// Keep track of loop labels, makes it easier to implement complex control flow
struct LoopContext {
    break_label: String,
    continue_label: String,
}

pub struct CodeGenerator {
    code: Vec<Instruction>,
    label_count: u32,
    next_addr: i32,
    scopes: Vec<HashMap<String, i32>>,
    loop_context_stack: Vec<LoopContext>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            label_count: 0,
            next_addr: 0,
            scopes: Vec::new(),
            loop_context_stack: Vec::new(),
        }
    }

    pub fn generate(&mut self, program: Program) -> Vec<Instruction> {
        self.emit(Instruction::CALL("main".to_string()));
        self.emit(Instruction::JUMP("exit".to_string()));

        for decl in program.declarations {
            match decl {
                Declaration::FunDecl(fun) => {
                    self.generate_fun_declaration(fun);
                },
                Declaration::VarDecl(var_decl) => {

                    self.enter_scope();

                    let array_size = var_decl.array_size.unwrap_or(1);
                    self.allocate_variable(&var_decl.id, array_size);
                    self.exit_scope();
                },
            }
        }

        self.emit(Instruction::LABEL("exit".to_string()));
        self.emit(Instruction::END);

        self.code.clone()
    }

    fn generate_fun_declaration(&mut self, fun: FunDeclaration) {
        self.emit(Instruction::LABEL(fun.id.clone()));

        self.enter_scope();

        for param in &fun.params {
            self.allocate_variable(&param.id, 1);
        }

        for param in fun.params.iter().rev() {
            let addr = self
                .lookup_variable(&param.id)
                .expect("Parameter not found");
            self.emit(Instruction::PUSH(addr));
            self.emit(Instruction::SAVE);
            self.emit(Instruction::POP);
        }

        self.generate_compound_stmt(fun.body);

        match fun.return_type {
            TypeSpecifier::Void => {
                self.emit(Instruction::RET);
            },
            TypeSpecifier::Int => {
                self.emit(Instruction::PUSH(0));
                self.emit(Instruction::RETV);
            },
        }

        self.exit_scope();
    }

    fn generate_compound_stmt(&mut self, comp: CompoundStmt) {
        self.enter_scope();

        for var_decl in comp.local_decls {
            let size = var_decl.array_size.unwrap_or(1);
            self.allocate_variable(&var_decl.id, size);
        }

        for stmt in comp.statements {
            self.generate_statement(stmt);
        }

        self.exit_scope();
    }

    fn generate_statement(&mut self, stmt: Statement) {
        use Statement::*;
        match stmt {
            ExpressionStmt(opt_expr) => {
                if let Some(expr) = opt_expr {
                    self.generate_expression(expr);
                    self.emit(Instruction::POP);

                }
            },
            CompoundStmt(comp) => {
                self.generate_compound_stmt(comp);
            },
            SelectionStmt { condition, then_stmt, else_stmt } => {
                let else_label = self.new_label("if_else");
                let end_label = self.new_label("if_end");

                self.generate_expression(condition);
                self.emit(Instruction::BRZ(if else_stmt.is_some() { else_label.clone() } else { end_label.clone() })); // Jump target depends on whether 'else' exists

                self.generate_statement(*then_stmt);
                if else_stmt.is_some() {
                    self.emit(Instruction::JUMP(end_label.clone()));
                    self.emit(Instruction::LABEL(else_label));
                    self.generate_statement(*else_stmt.unwrap());
                } else {
                   // If no else, BRZ already jumped to end_label
                }
                self.emit(Instruction::LABEL(end_label));
            },
            IterationStmt { condition, body } => {
                let start_label = self.new_label("while_start");
                let end_label = self.new_label("while_end");

                // Push context for break/continue
                self.loop_context_stack.push(LoopContext {
                    break_label: end_label.clone(),
                    continue_label: start_label.clone(),
                });

                self.emit(Instruction::LABEL(start_label.clone()));
                self.generate_expression(condition);
                self.emit(Instruction::BRZ(end_label.clone()));

                self.generate_statement(*body);
                self.emit(Instruction::JUMP(start_label));
                self.emit(Instruction::LABEL(end_label));

                // Pop context
                self.loop_context_stack.pop();
            },
            ReturnStmt(opt_expr) => {
                if let Some(expr) = opt_expr {
                    self.generate_expression(expr);
                    self.emit(Instruction::RETV);
                } else {
                    self.emit(Instruction::RET);
                }
            },
            OutputStmt(expr) => {
                match expr {
                    Expression::StringLit(s) => {
                        self.emit(Instruction::PRINT(Some(s)));
                    },
                    _ => {
                        self.generate_expression(expr);
                        self.emit(Instruction::PRINT(None));
                    }
                }
            },
            ForStmt { initializer, condition, update, body } => {
                let start_label = self.new_label("for_start");
                let update_label = self.new_label("for_update");
                let end_label = self.new_label("for_end");

                 self.loop_context_stack.push(LoopContext {
                    break_label: end_label.clone(),
                    continue_label: update_label.clone(),
                });

                if let Some(init_expr) = initializer {
                    self.generate_expression(*init_expr);
                    self.emit(Instruction::POP);
                }

                self.emit(Instruction::LABEL(start_label.clone()));
                if let Some(cond_expr) = condition {
                    self.generate_expression(*cond_expr);
                    self.emit(Instruction::BRZ(end_label.clone()));
                }

                self.generate_statement(*body);

                self.emit(Instruction::LABEL(update_label.clone()));
                if let Some(update_expr) = update {
                    self.generate_expression(*update_expr);
                    self.emit(Instruction::POP);
                }

                self.emit(Instruction::JUMP(start_label));

                self.emit(Instruction::LABEL(end_label));

                self.loop_context_stack.pop();
            },
            SwitchStmt { control_expr, cases, default_case } => {
                let end_label = self.new_label("switch_end");
                let mut case_labels = Vec::new();

                self.loop_context_stack.push(LoopContext {
                   break_label: end_label.clone(),
                   continue_label: "__invalid_continue_in_switch__".to_string(),
               });

                self.generate_expression(control_expr);

                for (i, case_stmt) in cases.iter().enumerate() {
                    let case_label = self.new_label(&format!("case_{}", i));
                    case_labels.push(case_label.clone());
                    self.emit(Instruction::DUP);
                    self.emit(Instruction::PUSH(case_stmt.value));
                    self.emit(Instruction::EQ);
                    self.emit(Instruction::BRT(case_label));
                }

                let default_label = self.new_label("switch_default");
                if default_case.is_some() {
                    self.emit(Instruction::JUMP(default_label.clone()));
                } else {
                    self.emit(Instruction::JUMP(end_label.clone()));
                }

                self.emit(Instruction::POP);

                for (i, case_stmt) in cases.into_iter().enumerate() {
                    self.emit(Instruction::LABEL(case_labels[i].clone()));
                    for stmt in case_stmt.body {
                        self.generate_statement(stmt);
                    }
                }

                if let Some(def_stmt) = default_case {
                   self.emit(Instruction::LABEL(default_label));
                   for stmt in def_stmt.body {
                       self.generate_statement(stmt);
                   }
                }

                self.emit(Instruction::LABEL(end_label));
                self.loop_context_stack.pop();
            },
            BreakStmt => {
                if let Some(context) = self.loop_context_stack.last() {
                     if context.break_label == "__invalid_continue_in_switch__" { 
                          panic!("'continue' is not valid inside a 'switch' statement");
                     }
                    self.emit(Instruction::JUMP(context.break_label.clone()));
                } else {
                    panic!("'break' statement used outside of a loop or switch");
                }
            },
            ContinueStmt => {
                 if let Some(context) = self.loop_context_stack.last() {
                     if context.continue_label == "__invalid_continue_in_switch__" {
                          panic!("'continue' is not valid inside a 'switch' statement");
                     }
                    self.emit(Instruction::JUMP(context.continue_label.clone()));
                } else {
                    panic!("'continue' statement used outside of a loop");
                }
            },
        }
    }

    fn generate_expression(&mut self, expr: Expression) {
        use Expression::*;
        match expr {
            Num(n) => {
                self.emit(Instruction::PUSH(n));
            },
            StringLit(_s) => {
                // This situation should only occur within an output statement.
                panic!("String literal expressions are only supported in output statements");
            },
            Var(var) => {
                self.generate_variable(var, false);
            },
            Assign { var, expr } => {
                self.generate_expression(*expr);
                self.generate_variable(var, true);
                self.emit(Instruction::SAVE);
            },
            Binary { left, op, right } => {
                self.generate_expression(*left);
                self.generate_expression(*right);
                match op {
                    BinaryOp::Add => self.emit(Instruction::ADD),
                    BinaryOp::Sub => self.emit(Instruction::SUB),
                    BinaryOp::Mul => self.emit(Instruction::MUL),
                    BinaryOp::Div => self.emit(Instruction::DIV),
                    BinaryOp::RelOp(op_str) => {
                        match op_str.as_str() {
                            "==" => self.emit(Instruction::EQ),
                            "!=" => self.emit(Instruction::NE),
                            "<"  => self.emit(Instruction::LT),
                            "<=" => self.emit(Instruction::LE),
                            ">"  => self.emit(Instruction::GT),
                            ">=" => self.emit(Instruction::GE),
                            _ => panic!("Unknown relational operator: {}", op_str),
                        }
                    },
                }
            },
            Call { id, args } => {
                if id == "input" {
                    self.emit(Instruction::READ);
                } else {
                    for arg in args {
                        self.generate_expression(arg);
                    }
                    self.emit(Instruction::CALL(id));
                }
            },
        }
    }

    fn generate_variable(&mut self, var: Var, for_assignment: bool) {
        use Var::*;
        match var {
            Id(name) => {
                let addr = self.lookup_variable(&name)
                    .unwrap_or_else(|| panic!("Undefined variable: {}", name));
                if for_assignment {
                    self.emit(Instruction::PUSH(addr));
                } else {
                    self.emit(Instruction::PUSH(addr));
                    self.emit(Instruction::LOAD);
                }
            },
            ArrayAccess { id, index } => {
                let base_addr = self.lookup_variable(&id)
                    .unwrap_or_else(|| panic!("Undefined array: {}", id));
                self.generate_expression(*index);
                self.emit(Instruction::PUSH(base_addr));
                self.emit(Instruction::ADD);
                if !for_assignment {
                    self.emit(Instruction::LOAD);
                }
            },
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn allocate_variable(&mut self, name: &str, size: usize) -> i32 {
        let addr = self.next_addr;
        self.next_addr += size as i32;
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), addr);
        } else {
            panic!("No active scope when allocating variable");
        }
        addr
    }

    fn lookup_variable(&self, name: &str) -> Option<i32> {
        for scope in self.scopes.iter().rev() {
            if let Some(&addr) = scope.get(name) {
                return Some(addr);
            }
        }
        None
    }

    fn emit(&mut self, instr: Instruction) {
        self.code.push(instr);
    }

    fn new_label(&mut self, base: &str) -> String {
        let label = format!("{}_{}", base, self.label_count);
        self.label_count += 1;
        label
    }
}
