use crate::ast::TypeSpecifier;
use crate::ast::*;
use crate::stack_machine::Instruction;
use std::collections::HashMap;

struct LoopContext {
    break_label: String,
    continue_label: String,
}

#[derive(Debug, Clone, Copy)]
struct ScopeEntry {
    address: i32,
    is_ref: bool,
}
pub struct CodeGenerator {
    code: Vec<Instruction>,
    label_count: u32,
    next_addr: i32,
    scopes: Vec<HashMap<String, ScopeEntry>>,
    loop_context_stack: Vec<LoopContext>,
    fun_signatures: HashMap<String, (Vec<Param>, TypeSpecifier)>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            label_count: 0,
            next_addr: 0,
            scopes: Vec::new(),
            loop_context_stack: Vec::new(),
            fun_signatures: HashMap::new(),
        }
    }

    // Stores the parameters and return type for a function.
    fn register_function_signature(&mut self, fun: &FunDeclaration) {
        if !self.fun_signatures.contains_key(&fun.id) {
            self.fun_signatures.insert(
                fun.id.clone(),
                (fun.params.clone(), fun.return_type.clone()),
            );
        }
    }

    // Retrieves the return type of a function.
    fn get_function_return_type(&self, fun_id: &str) -> Option<TypeSpecifier> {
        self.fun_signatures
            .get(fun_id)
            .map(|(_, ret_type)| ret_type.clone())
    }

    pub fn generate(&mut self, program: Program) -> Vec<Instruction> {
        self.emit(Instruction::CALL("main".to_string(), 0));
        self.emit(Instruction::JUMP("exit".to_string()));

        // First pass: Collect function signatures
        for decl in &program.declarations {
            if let Declaration::FunDecl(fun) = decl {
                self.register_function_signature(fun);
            }
        }

        // Second pass: Generate code
        for decl in program.declarations {
            match decl {
                Declaration::FunDecl(fun) => {
                    // Ensure signature is registered (should be from first pass)
                    self.register_function_signature(&fun);
                    self.generate_fun_declaration(fun);
                }
                Declaration::VarDecl(var_decl) => {
                    // Handle global variables (Needs proper implementation if globals are needed)
                    println!(
                        "Warning: Global variable declarations are not fully supported yet ({})",
                        var_decl.id
                    );
                }
            }
        }

        self.emit(Instruction::LABEL("exit".to_string()));
        self.emit(Instruction::END);

        self.code.clone()
    }

    fn generate_fun_declaration(&mut self, fun: FunDeclaration) {
        self.emit(Instruction::LABEL(fun.id.clone()));
        self.enter_scope();

        // Allocate space for parameters
        for param in &fun.params {
            self.allocate_variable(&param.id, param.is_ref);
        }

        // Pop arguments passed by caller and save them into parameter locations
        for param in fun.params.iter().rev() {
            let entry = self
                .lookup_variable_entry(&param.id)
                .expect("Parameter not found during save");
            self.emit(Instruction::PUSH(entry.address));
            self.emit(Instruction::SAVE);
            self.emit(Instruction::POP);
        }

        self.generate_compound_stmt(fun.body);

        // Handle return
        match fun.return_type {
            TypeSpecifier::Void => {
                self.emit(Instruction::RET);
            }
            TypeSpecifier::Int => {
                // Default return 0 if control reaches the end of a non-void function
                self.emit(Instruction::PUSH(0));
                self.emit(Instruction::RETV);
            }
        }

        self.exit_scope();
    }

    fn generate_compound_stmt(&mut self, comp: CompoundStmt) {
        self.enter_scope();
        for var_decl in comp.local_decls {
            self.allocate_variable(&var_decl.id, false);
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
                    let mut _arg_count = 0;
                    let mut returns_int = false;
                    let mut is_call = false;

                    if let Expression::Call { ref id, ref args } = expr {
                        is_call = true;
                        if id == "input" {
                            _arg_count = 0;
                            returns_int = true;
                        } else if let Some(ret_type) = self.get_function_return_type(id) {
                            _arg_count = args.len();
                            returns_int = matches!(ret_type, TypeSpecifier::Int);
                        } else {
                            println!(
                                "Warning: Assuming void return for unknown function call '{}'",
                                id
                            );
                            _arg_count = args.len();
                            returns_int = false;
                        }
                    }

                    // Generate the expression code (e.g., pushes args, emits CALL)
                    self.generate_expression(expr);

                    // Post-execution cleanup based on expression type
                    if is_call {
                        if returns_int {
                            self.emit(Instruction::POP);
                        }
                    } else {
                        self.emit(Instruction::POP);
                    }
                }
            } // End ExpressionStmt arm
            CompoundStmt(comp) => {
                self.generate_compound_stmt(comp);
            }
            SelectionStmt {
                condition,
                then_stmt,
                else_stmt,
            } => {
                let else_label = self.new_label("if_else");
                let end_label = self.new_label("if_end");
                let target_label = if else_stmt.is_some() {
                    else_label.clone()
                } else {
                    end_label.clone()
                };

                self.generate_expression(condition);
                self.emit(Instruction::BRZ(target_label));

                self.generate_statement(*then_stmt);
                if else_stmt.is_some() {
                    self.emit(Instruction::JUMP(end_label.clone()));
                    self.emit(Instruction::LABEL(else_label));
                    self.generate_statement(*else_stmt.unwrap());
                }

                self.emit(Instruction::LABEL(end_label));
            }
            IterationStmt { condition, body } => {
                let start_label = self.new_label("while_start");
                let end_label = self.new_label("while_end");

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

                self.loop_context_stack.pop();
            }
            ReturnStmt(opt_expr) => {
                if let Some(expr) = opt_expr {
                    self.generate_expression(expr);
                    self.emit(Instruction::RETV);
                } else {
                    self.emit(Instruction::RET);
                }
            }
            OutputStmt(expr) => match expr {
                Expression::StringLit(s) => {
                    self.emit(Instruction::PRINT(Some(s)));
                }
                _ => {
                    self.generate_expression(expr);
                    self.emit(Instruction::PRINT(None));
                }
            },
            ForStmt {
                initializer,
                condition,
                update,
                body,
            } => {
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
            }
            SwitchStmt {
                control_expr,
                cases,
                default_case,
            } => {
                let end_label = self.new_label("switch_end");
                let default_label = self.new_label("switch_default");
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

                if default_case.is_some() {
                    self.emit(Instruction::JUMP(default_label.clone()));
                } else {
                    self.emit(Instruction::JUMP(end_label.clone()));
                }

                self.emit(Instruction::POP); // Pop the control value

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
            }
            BreakStmt => {
                if let Some(context) = self.loop_context_stack.last() {
                    // No need to check continue_label here, break is valid in switch
                    self.emit(Instruction::JUMP(context.break_label.clone()));
                } else {
                    panic!("'break' statement used outside of a loop or switch");
                }
            }
            ContinueStmt => {
                if let Some(context) = self.loop_context_stack.last() {
                    if context.continue_label == "__invalid_continue_in_switch__" {
                        panic!("'continue' statement used outside of a loop");
                    }
                    self.emit(Instruction::JUMP(context.continue_label.clone()));
                } else {
                    panic!("'continue' statement used outside of a loop");
                }
            }
        }
    }

    fn generate_expression(&mut self, expr: Expression) {
        use Expression::*;
        match expr {
            Num(n) => {
                self.emit(Instruction::PUSH(n));
            }
            StringLit(_s) => {
                panic!(
                    "String literal expressions are only supported directly in output statements"
                );
            }
            Var(var) => {
                self.generate_variable_access(var, false);
            }
            Assign { var, expr } => {
                self.generate_expression(*expr);
                self.generate_variable_access(var, true);
                self.emit(Instruction::SAVE);
            }
            Binary { left, op, right } => {
                self.generate_expression(*left);
                self.generate_expression(*right);
                match op {
                    BinaryOp::Add => self.emit(Instruction::ADD),
                    BinaryOp::Sub => self.emit(Instruction::SUB),
                    BinaryOp::Mul => self.emit(Instruction::MUL),
                    BinaryOp::Div => self.emit(Instruction::DIV),
                    BinaryOp::RelOp(op_str) => match op_str.as_str() {
                        "==" => self.emit(Instruction::EQ),
                        "!=" => self.emit(Instruction::NE),
                        "<" => self.emit(Instruction::LT),
                        "<=" => self.emit(Instruction::LE),
                        ">" => self.emit(Instruction::GT),
                        ">=" => self.emit(Instruction::GE),
                        _ => panic!("Unknown relational operator: {}", op_str),
                    },
                }
            }
            Call { id, args } => {
                if id == "input" {
                    self.emit(Instruction::READ);
                } else {
                    let (signature_params, _) = self 
                        .fun_signatures
                        .get(&id)
                        .cloned()
                        .unwrap_or_else(|| panic!("Call to undefined function: {}", id));

                    if args.len() != signature_params.len() {
                        panic!(
                            "Incorrect number of arguments for function call '{}'. Expected {}, got {}.",
                            id,
                            signature_params.len(),
                            args.len()
                        );
                    }
                    let arg_count = args.len();

                    for (arg_expr, param_info) in args.into_iter().zip(signature_params.iter()) {
                        if param_info.is_ref {
                            match arg_expr {
                                Expression::Var(var) => {
                                    self.generate_variable_access(var, true);
                                }
                                _ => panic!(
                                    "Argument passed by reference must be a variable, got {:?}",
                                    arg_expr
                                ),
                            }
                        } else {
                            self.generate_expression(arg_expr);
                        }
                    }
                    self.emit(Instruction::CALL(id, arg_count));
                }
            }
        }
    }

    fn generate_variable_access(&mut self, var: Var, push_address: bool) {
        use Var::*;
        match var {
            Id(name) => {
                let entry = self
                    .lookup_variable_entry(&name)
                    .unwrap_or_else(|| panic!("Undefined variable: {}", name));

                if entry.is_ref {
                    self.emit(Instruction::PUSH(entry.address));
                    self.emit(Instruction::LOAD);
                    // Stack now has the address of the original variable.

                    if !push_address {
                        self.emit(Instruction::LOAD);
                    }
                } else {
                    // Push the variable's own address.
                    self.emit(Instruction::PUSH(entry.address));

                    if !push_address {
                        self.emit(Instruction::LOAD);
                    }
                }
            }
            ArrayAccess { id, index } => {
                let entry = self
                    .lookup_variable_entry(&id)
                    .unwrap_or_else(|| panic!("Undefined array: {}", id));

                if entry.is_ref {
                    panic!(
                        "Accessing elements of array passed by reference '&' is not supported yet."
                    );
                }

                // Push base address of the array
                self.emit(Instruction::PUSH(entry.address));
                // Evaluate the index expression
                self.generate_expression(*index);
                // Calculate element address (base + index)
                self.emit(Instruction::ADD);

                if !push_address {
                    self.emit(Instruction::LOAD);
                }
            }
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn allocate_variable(&mut self, name: &str, is_ref: bool) -> ScopeEntry {
        let addr = self.next_addr;
        self.next_addr += 1; // Allocate 1 slot per var/param
        let entry = ScopeEntry {
            address: addr,
            is_ref,
        };
        if let Some(scope) = self.scopes.last_mut() {
            if scope.insert(name.to_string(), entry).is_some() {
                // println!("Warning: Shadowing variable '{}'", name);
            }
        } else {
            panic!("No active scope when allocating variable");
        }
        entry
    }

    fn lookup_variable_entry(&self, name: &str) -> Option<ScopeEntry> {
        for scope in self.scopes.iter().rev() {
            if let Some(&entry) = scope.get(name) {
                return Some(entry);
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
