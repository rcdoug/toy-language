use crate::ast::*;
use crate::lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn current_token(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn consume(&mut self) -> Token {
        let tok = self.tokens.get(self.pos).expect("Unexpected end of tokens").clone();
        self.pos += 1;
        tok
    }

    fn expect(&mut self, expected: &Token) {
        let tok = self.consume();
        if &tok != expected {
            panic!("Expected {:?} but got {:?}", expected, tok);
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut declarations = Vec::new();
        while self.pos < self.tokens.len() {
            declarations.push(self.parse_declaration());
        }
        Program { declarations }
    }

    fn parse_declaration(&mut self) -> Declaration {
        let type_spec = self.parse_type_specifier();
        match self.current_token() {
            Some(Token::Id(_)) => {
                let id = if let Token::Id(s) = self.consume() {
                    s
                } else { unreachable!() };
                match self.current_token() {
                    Some(Token::Semicolon) | Some(Token::LBracket) => {
                        // var-declaration
                        Declaration::VarDecl(self.parse_var_declaration_with(type_spec, id))
                    },
                    Some(Token::LParen) => {
                        // fun-declaration
                        Declaration::FunDecl(self.parse_fun_declaration_with(type_spec, id))
                    },
                    _ => panic!("Unexpected token after identifier in declaration"),
                }
            },
            _ => panic!("Expected identifier in declaration"),
        }
    }

    fn parse_type_specifier(&mut self) -> TypeSpecifier {
        match self.consume() {
            Token::Int => TypeSpecifier::Int,
            Token::Void => TypeSpecifier::Void,
            tok => panic!("Unexpected token in type specifier: {:?}", tok),
        }
    }

    fn parse_var_declaration_with(&mut self, type_spec: TypeSpecifier, id: String) -> VarDeclaration {
        let mut array_size = None;
        if let Some(Token::LBracket) = self.current_token() {
            self.consume(); // consume '['
            match self.consume() {
                Token::Num(n) => array_size = Some(n as usize),
                tok => panic!("Expected number for array size but got {:?}", tok),
            }
            self.expect(&Token::RBracket);
        }
        self.expect(&Token::Semicolon);
        VarDeclaration { type_spec, id, array_size }
    }

    fn parse_fun_declaration_with(&mut self, return_type: TypeSpecifier, id: String) -> FunDeclaration {
        self.expect(&Token::LParen);
        let params = self.parse_params();
        self.expect(&Token::RParen);
        let body = self.parse_compound_stmt();
        FunDeclaration { return_type, id, params, body }
    }

    fn parse_params(&mut self) -> Vec<Param> {
        // params := param-list | void | empty
        match self.current_token() {
            Some(Token::Void) => {
                self.consume();
                Vec::new()
            },
            Some(Token::RParen) => Vec::new(),
            _ => self.parse_param_list(),
        }
    }

    fn parse_param_list(&mut self) -> Vec<Param> {
        let mut params = Vec::new();
        params.push(self.parse_param());
        while let Some(Token::Comma) = self.current_token() {
            self.consume();
            params.push(self.parse_param());
        }
        params
    }

    fn parse_param(&mut self) -> Param {
        let type_spec = self.parse_type_specifier();
        let id = if let Token::Id(s) = self.consume() { s } else { panic!("Expected identifier in parameter") };
        let mut is_array = false;
        if let Some(Token::LBracket) = self.current_token() {
            self.consume(); // consume '['
            self.expect(&Token::RBracket);
            is_array = true;
        }
        Param { type_spec, id, is_array }
    }

    fn parse_compound_stmt(&mut self) -> CompoundStmt {
        self.expect(&Token::LBrace);
        let local_decls = self.parse_local_declarations();
        let statements = self.parse_statement_list();
        self.expect(&Token::RBrace);
        CompoundStmt { local_decls, statements }
    }

    fn parse_local_declarations(&mut self) -> Vec<VarDeclaration> {
        let mut decls = Vec::new();
        while let Some(tok) = self.current_token() {
            match tok {
                Token::Int | Token::Void => {
                    let type_spec = self.parse_type_specifier();
                    if let Token::Id(id) = self.consume() {
                        decls.push(self.parse_var_declaration_with(type_spec, id));
                    } else {
                        panic!("Expected id in local declaration");
                    }
                },
                _ => break,
            }
        }
        decls
    }

    fn parse_statement_list(&mut self) -> Vec<Statement> {
        let mut stmts = Vec::new();
        while let Some(tok) = self.current_token() {
            if let Token::RBrace = tok { break; }
            stmts.push(self.parse_statement());
        }
        stmts
    }

    fn parse_statement(&mut self) -> Statement {
        match self.current_token() {
            Some(Token::LBrace) => Statement::CompoundStmt(self.parse_compound_stmt()),
            Some(Token::If) => self.parse_selection_stmt(),
            Some(Token::While) => self.parse_iteration_stmt(),
            Some(Token::Return) => self.parse_return_stmt(),
            Some(Token::Output) => self.parse_output_stmt(),
            Some(Token::For) => self.parse_for_stmt(),
            Some(Token::Switch) => self.parse_switch_stmt(),
            Some(Token::Break) => self.parse_break_stmt(),
            Some(Token::Continue) => self.parse_continue_stmt(),
            _ => self.parse_expression_stmt(),
        }
    }

    fn parse_output_stmt(&mut self) -> Statement {
        // output( expression );
        self.expect(&Token::Output);
        self.expect(&Token::LParen);
        let expr = self.parse_expression();
        self.expect(&Token::RParen);
        self.expect(&Token::Semicolon);
        Statement::OutputStmt(expr)
    }

    fn parse_expression_stmt(&mut self) -> Statement {
        // expression-stmt := expression ; | ;
        if let Some(Token::Semicolon) = self.current_token() {
            self.consume();
            Statement::ExpressionStmt(None)
        } else {
            let expr = self.parse_expression();
            self.expect(&Token::Semicolon);
            Statement::ExpressionStmt(Some(expr))
        }
    }

    fn parse_selection_stmt(&mut self) -> Statement {
        // selection-stmt := if ( expression ) statement [else statement]
        self.expect(&Token::If);
        self.expect(&Token::LParen);
        let condition = self.parse_expression();
        self.expect(&Token::RParen);
        let then_stmt = Box::new(self.parse_statement());
        let else_stmt = if let Some(Token::Else) = self.current_token() {
            self.consume();
            Some(Box::new(self.parse_statement()))
        } else {
            None
        };
        Statement::SelectionStmt { condition, then_stmt, else_stmt }
    }

    fn parse_iteration_stmt(&mut self) -> Statement {
        // iteration-stmt := while ( expression ) statement
        self.expect(&Token::While);
        self.expect(&Token::LParen);
        let condition = self.parse_expression();
        self.expect(&Token::RParen);
        let body = Box::new(self.parse_statement());
        Statement::IterationStmt { condition, body }
    }

    fn parse_return_stmt(&mut self) -> Statement {
        // return-stmt := return ; | return expression ;
        self.expect(&Token::Return);
        if let Some(Token::Semicolon) = self.current_token() {
            self.consume();
            Statement::ReturnStmt(None)
        } else {
            let expr = self.parse_expression();
            self.expect(&Token::Semicolon);
            Statement::ReturnStmt(Some(expr))
        }
    }

    fn parse_expression(&mut self) -> Expression {
        // expression := var = expression | simple-expression
        let expr = self.parse_simple_expression();
        if let Some(Token::Assign) = self.current_token() {
            self.consume(); // consume '='
            if let Expression::Var(var) = expr {
                let rhs = self.parse_expression();
                Expression::Assign { var, expr: Box::new(rhs) }
            } else {
                panic!("Left side of assignment must be a variable");
            }
        } else {
            expr
        }
    }

    fn parse_simple_expression(&mut self) -> Expression {
        // simple-expression := additive-expression [relop additive-expression]
        let left = self.parse_additive_expression();
        if let Some(tok) = self.current_token() {
            match tok {
                Token::Le | Token::Lt | Token::Gt | Token::Ge | Token::Eq | Token::Ne => {
                    let op = match self.consume() {
                        Token::Le => BinaryOp::RelOp("<=".to_string()),
                        Token::Lt => BinaryOp::RelOp("<".to_string()),
                        Token::Gt => BinaryOp::RelOp(">".to_string()),
                        Token::Ge => BinaryOp::RelOp(">=".to_string()),
                        Token::Eq => BinaryOp::RelOp("==".to_string()),
                        Token::Ne => BinaryOp::RelOp("!=".to_string()),
                        _ => unreachable!(),
                    };
                    let right = self.parse_additive_expression();
                    Expression::Binary { left: Box::new(left), op, right: Box::new(right) }
                },
                _ => left,
            }
        } else {
            left
        }
    }

    fn parse_additive_expression(&mut self) -> Expression {
        let mut expr = self.parse_term();
        while let Some(tok) = self.current_token() {
            match tok {
                Token::Plus | Token::Minus => {
                    let op = match self.consume() {
                        Token::Plus => BinaryOp::Add,
                        Token::Minus => BinaryOp::Sub,
                        _ => unreachable!(),
                    };
                    let right = self.parse_term();
                    expr = Expression::Binary { left: Box::new(expr), op, right: Box::new(right) };
                },
                _ => break,
            }
        }
        expr
    }

    fn parse_term(&mut self) -> Expression {
        let mut expr = self.parse_factor();
        while let Some(tok) = self.current_token() {
            match tok {
                Token::Star | Token::Slash => {
                    let op = match self.consume() {
                        Token::Star => BinaryOp::Mul,
                        Token::Slash => BinaryOp::Div,
                        _ => unreachable!(),
                    };
                    let right = self.parse_factor();
                    expr = Expression::Binary { left: Box::new(expr), op, right: Box::new(right) };
                },
                _ => break,
            }
        }
        expr
    }

    fn parse_factor(&mut self) -> Expression {
        match self.current_token() {
            Some(Token::LParen) => {
                self.consume();
                let expr = self.parse_expression();
                self.expect(&Token::RParen);
                expr
            },
            Some(Token::StringLit(_)) => {
                if let Token::StringLit(s) = self.consume() {
                    Expression::StringLit(s)
                } else { unreachable!() }
            },
            Some(Token::Input) => {
                self.consume(); // consume 'input'
                self.expect(&Token::LParen);
                self.expect(&Token::RParen);
                Expression::Call { id: "input".to_string(), args: Vec::new() }
            },
            Some(Token::Id(_)) => {
                // Look ahead to determine if a call or a variable.
                if self.look_ahead(1) == Some(&Token::LParen) {
                    self.parse_call()
                } else {
                    self.parse_var()
                }
            },
            Some(Token::Num(_)) => {
                if let Token::Num(n) = self.consume() { Expression::Num(n) } else { unreachable!() }
            },
            tok => panic!("Unexpected token in factor: {:?}", tok),
        }
    }

    fn parse_call(&mut self) -> Expression {
        // call := ID ( args )
        let id = if let Token::Id(s) = self.consume() { s } else { panic!("Expected function name"); };
        self.expect(&Token::LParen);
        let args = self.parse_args();
        self.expect(&Token::RParen);
        Expression::Call { id, args }
    }

    fn parse_args(&mut self) -> Vec<Expression> {
        // args := arg-list | empty
        if let Some(Token::RParen) = self.current_token() {
            Vec::new()
        } else {
            self.parse_arg_list()
        }
    }

    fn parse_arg_list(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();
        args.push(self.parse_expression());
        while let Some(Token::Comma) = self.current_token() {
            self.consume();
            args.push(self.parse_expression());
        }
        args
    }

    fn parse_var(&mut self) -> Expression {
        // var := ID | ID [ expression ]
        let id = if let Token::Id(s) = self.consume() { s } else { panic!("Expected identifier"); };
        if let Some(Token::LBracket) = self.current_token() {
            self.consume(); // consume '['
            let index = self.parse_expression();
            self.expect(&Token::RBracket);
            Expression::Var(Var::ArrayAccess { id, index: Box::new(index) })
        } else {
            Expression::Var(Var::Id(id))
        }
    }

    fn look_ahead(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.pos + n)
    }

    // for ( [expression] ; [expression] ; [expression] ) statement
    fn parse_for_stmt(&mut self) -> Statement {
        self.expect(&Token::For);
        self.expect(&Token::LParen);

        // Initializer
        let initializer = if self.current_token() == Some(&Token::Semicolon) {
            None
        } else {
            Some(Box::new(self.parse_expression()))
        };
        self.expect(&Token::Semicolon);

        // Condition
        let condition = if self.current_token() == Some(&Token::Semicolon) {
            None // Infinite loop / break needed. - treat as true
        } else {
            Some(Box::new(self.parse_expression()))
        };
        self.expect(&Token::Semicolon);

        let update = if self.current_token() == Some(&Token::RParen) {
            None
        } else {
            Some(Box::new(self.parse_expression()))
        };
        self.expect(&Token::RParen);

        // Body
        let body = Box::new(self.parse_statement());

        Statement::ForStmt { initializer, condition, update, body }
    }

    // switch ( expression ) { case-list [default] }
    fn parse_switch_stmt(&mut self) -> Statement {
        self.expect(&Token::Switch);
        self.expect(&Token::LParen);
        let control_expr = self.parse_expression();
        self.expect(&Token::RParen);
        self.expect(&Token::LBrace);

        let mut cases = Vec::new();
        let mut default_case = None;

        while let Some(tok) = self.current_token() {
            match tok {
                Token::Case => {
                    cases.push(self.parse_case_stmt());
                },
                Token::Default => {
                    if default_case.is_some() {
                        panic!("Switch statement can only have one default case");
                    }
                    default_case = Some(self.parse_default_stmt());
                },
                Token::RBrace => break,
                _ => panic!("Expected 'case', 'default', or '}}' in switch statement, got {:?}", tok),
            }
        }
        self.expect(&Token::RBrace);

        Statement::SwitchStmt { control_expr, cases, default_case }
    }

    // case constant : statement-list
    fn parse_case_stmt(&mut self) -> CaseStmt {
        self.expect(&Token::Case);
        let value = match self.consume() {
            Token::Num(n) => n,
            tok => panic!("Expected integer constant for case label, got {:?}", tok),
        };
        self.expect(&Token::Colon);

        let mut body = Vec::new();
        while let Some(tok) = self.current_token() {
            match tok {
                Token::Case | Token::Default | Token::RBrace => break,
                _ => body.push(self.parse_statement()),
            }
        }

        CaseStmt { value, body }
    }

    // default : statement-list
    fn parse_default_stmt(&mut self) -> DefaultStmt {
        self.expect(&Token::Default);
        self.expect(&Token::Colon);

        let mut body = Vec::new();
        while let Some(tok) = self.current_token() {
            match tok {
                Token::Case | Token::Default | Token::RBrace => break,
                _ => body.push(self.parse_statement()),
            }
        }

        DefaultStmt { body }
    }
    
    // break
    fn parse_break_stmt(&mut self) -> Statement {
        self.expect(&Token::Break);
        self.expect(&Token::Semicolon);
        Statement::BreakStmt
    }

    // continue
    fn parse_continue_stmt(&mut self) -> Statement {
        self.expect(&Token::Continue);
        self.expect(&Token::Semicolon);
        Statement::ContinueStmt
    }
}
