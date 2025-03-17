use crate::tokenizer::Token;

#[derive(Debug)]
pub enum ASTNode {
    Num(i32),
    BinOp {
        left: Box<ASTNode>,
        op: Token,
        right: Box<ASTNode>,
    },
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn current_token(&self) -> &Token {
        if self.pos < self.tokens.len() {
            &self.tokens[self.pos]
        } else {
            &Token::EOF  // Prevent out-of-bounds access
        }
    }    

    fn consume(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    fn parse_factor(&mut self) -> ASTNode {
        match self.current_token() {
            Token::Num(value) => {
                let node = ASTNode::Num(*value);
                self.consume();
                node
            }
            Token::LParen => {
                self.consume();
                let node = self.parse_expr();
                if *self.current_token() != Token::RParen {
                    panic!("Expected ')'");
                }
                self.consume();
                node
            }
            _ => panic!("Unexpected token in factor"),
        }
    }

    fn parse_term(&mut self) -> ASTNode {
        let mut node = self.parse_factor();

        while matches!(self.current_token(), Token::Mul | Token::Div) {
            let op = self.current_token().clone();
            self.consume();
            node = ASTNode::BinOp {
                left: Box::new(node),
                op,
                right: Box::new(self.parse_factor()),
            };
        }

        node
    }

    fn parse_expr(&mut self) -> ASTNode {
        let mut node = self.parse_term();

        while matches!(self.current_token(), Token::Plus | Token::Minus) {
            let op = self.current_token().clone();
            self.consume();
            node = ASTNode::BinOp {
                left: Box::new(node),
                op,
                right: Box::new(self.parse_term()),
            };
        }

        node
    }

    pub fn parse(&mut self) -> ASTNode {
        self.parse_expr()
    }
}
