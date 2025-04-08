#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords
    Int,
    Void,
    If,
    Else,
    While,
    Return,
    Output,
    Input,

    // Identifiers and Numbers
    Id(String),
    Num(i32),
    StringLit(String),

    // Operators and punctuation
    Plus,      
    Minus,     
    Star,       
    Slash,       
    Le,        
    Lt,       
    Gt,         
    Ge,          
    Eq,         
    Ne,          
    Assign,       
    Semicolon,    
    Comma,      
    LParen,     
    RParen,   
    LBrace,       
    RBrace,      
    LBracket,   
    RBracket,  
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self { input: input.chars().collect(), pos: 0 }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        if self.pos >= self.input.len() {
            return None;
        }
        let ch = self.input[self.pos];
        
        // Tried to handle string literals from the original code in the stack machine target file, didn't really work.
        // String concatonation was too much of a pain 
        if ch == '"' {
            self.pos += 1;
            let start = self.pos;
            while self.pos < self.input.len() && self.input[self.pos] != '"' {
                self.pos += 1;
            }
            if self.pos == self.input.len() {
                panic!("Unterminated string literal");
            }
            let string_val: String = self.input[start..self.pos].iter().collect();
            self.pos += 1;
            return Some(Token::StringLit(string_val));
        }
        
        match ch {
            '+' => { self.pos += 1; Some(Token::Plus) }
            '-' => { self.pos += 1; Some(Token::Minus) }
            '*' => { self.pos += 1; Some(Token::Star) }
            '/' => { self.pos += 1; Some(Token::Slash) }
            ';' => { self.pos += 1; Some(Token::Semicolon) }
            ',' => { self.pos += 1; Some(Token::Comma) }
            '(' => { self.pos += 1; Some(Token::LParen) }
            ')' => { self.pos += 1; Some(Token::RParen) }
            '{' => { self.pos += 1; Some(Token::LBrace) }
            '}' => { self.pos += 1; Some(Token::RBrace) }
            '[' => { self.pos += 1; Some(Token::LBracket) }
            ']' => { self.pos += 1; Some(Token::RBracket) }
            '=' => {
                self.pos += 1;
                if self.peek() == Some('=') {
                    self.pos += 1;
                    Some(Token::Eq)
                } else {
                    Some(Token::Assign)
                }
            }
            '!' => {
                self.pos += 1;
                if self.peek() == Some('=') {
                    self.pos += 1;
                    Some(Token::Ne)
                } else {
                    panic!("Unexpected character: !")
                }
            }
            '<' => {
                self.pos += 1;
                if self.peek() == Some('=') {
                    self.pos += 1;
                    Some(Token::Le)
                } else {
                    Some(Token::Lt)
                }
            }
            '>' => {
                self.pos += 1;
                if self.peek() == Some('=') {
                    self.pos += 1;
                    Some(Token::Ge)
                } else {
                    Some(Token::Gt)
                }
            }
            c if c.is_ascii_digit() => Some(self.number()),
            c if Self::is_id_start(c) => Some(self.identifier()),
            _ => panic!("Unrecognized character: {}", ch),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() && self.input[self.pos].is_whitespace() {
            self.pos += 1;
        }
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    fn is_id_start(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_id_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn number(&mut self) -> Token {
        let start = self.pos;
        while self.pos < self.input.len() && self.input[self.pos].is_ascii_digit() {
            self.pos += 1;
        }
        let num_str: String = self.input[start..self.pos].iter().collect();
        let value = num_str.parse::<i32>().unwrap();
        Token::Num(value)
    }

    fn identifier(&mut self) -> Token {
        let start = self.pos;
        while self.pos < self.input.len() && Self::is_id_char(self.input[self.pos]) {
            self.pos += 1;
        }
        let ident: String = self.input[start..self.pos].iter().collect();
        match ident.as_str() {
            "int"    => Token::Int,
            "void"   => Token::Void,
            "if"     => Token::If,
            "else"   => Token::Else,
            "while"  => Token::While,
            "return" => Token::Return,
            "output" => Token::Output,
            "input"  => Token::Input,
            _        => Token::Id(ident),
        }
    }
}
