#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Num(i32),
    Plus,
    Minus,
    Mul,
    Div,
    LParen,
    RParen,
    EOF,
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        if self.pos < self.input.len() {
            let ch = self.input[self.pos];
            self.pos += 1;
            Some(ch)
        } else {
            None
        }
    }

    fn peek_char(&self) -> Option<char> {
        if self.pos < self.input.len() {
            Some(self.input[self.pos])
        } else {
            None
        }
    }

    pub fn next_token(&mut self) -> Token {
        while let Some(ch) = self.peek_char() {
            match ch {
                '0'..='9' => return self.lex_number(),
                '+' => {
                    self.pos += 1;
                    return Token::Plus;
                }
                '-' => {
                    self.pos += 1;
                    return Token::Minus;
                }
                '*' => {
                    self.pos += 1;
                    return Token::Mul;
                }
                '/' => {
                    self.pos += 1;
                    return Token::Div;
                }
                '(' => {
                    self.pos += 1;
                    return Token::LParen;
                }
                ')' => {
                    self.pos += 1;
                    return Token::RParen;
                }
                ' ' | '\t' | '\n' => {
                    self.pos += 1;
                    continue;
                }
                _ => panic!("Unexpected character: {}", ch),
            }
        }
        Token::EOF
    }

    fn lex_number(&mut self) -> Token {
        let mut num = 0;
        while let Some(ch) = self.peek_char() {
            if ch.is_digit(10) {
                num = num * 10 + (ch as i32 - '0' as i32);
                self.pos += 1;
            } else {
                break;
            }
        }
        Token::Num(num)
    }
}
