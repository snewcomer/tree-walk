extern crate regex;
use regex::Regex;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum LexemeKind {
    // Single-character tokens.
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

    // One or two character tokens.
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,

    // Literals.
    IDENTIFIER(String),
    STRING(String),
    NUMBER(f64),

    // Keywords.
    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

    EOF
}

#[derive(Debug, PartialEq)]
pub struct Token {
    // line: usize,
    pub lexeme: LexemeKind,
}

pub struct Scanner {
    cursor: usize,
    chars: Vec<char>,
}

// Lexer
impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            // time and space higher with collect
            chars: source.chars().collect(),
            cursor: 0
        }
    }

    fn advance(&mut self) {
        self.cursor += 1;
    }

    fn current_char(&self) -> Option<&char> {
        self.chars.get(self.cursor)
    }

    fn current_char_unsafe(&self) -> char {
        self.chars[self.cursor]
    }

    fn peek_next(&self) -> Option<&char> {
        self.chars.get(self.cursor+1)
    }

    fn is_finished(&self) -> bool {
        self.cursor >= self.chars.len()
    }

    fn number_boundary(&mut self) -> f64 {
        let mut buffer = String::new();
        while self.current_char().is_some() {
            let c = self.current_char_unsafe();
            match c {
                add if is_number(add) || add == '.' => {
                    buffer.push(add.to_owned());
                    self.advance();
                }
                _ => break,
            }
        }

        buffer.parse().unwrap()
    }

    fn word_boundary(&mut self) -> String {
        // first was ". next char is potentially the word
        self.advance();
        let mut buffer = String::new();
        while self.peek_next().is_some() {
            let c = self.current_char();
            match c {
                Some(&'"') => break,
                Some(&add) => {
                    buffer.push(add.to_owned());
                    self.advance();
                }
                None => break,
            }
        }

        buffer
    }

    fn identifier_boundary(&mut self) -> LexemeKind {
        let mut buffer = String::new();
        while self.current_char().is_some() {
            let c = self.current_char_unsafe();
            match c {
                add if is_number(add) || is_valid_ident(add) => {
                    buffer.push(add.to_owned());
                    self.advance();
                }
                _ => break,
            }
        };

        match buffer.as_str() {
          "and" => LexemeKind::AND,
          "class" => LexemeKind::CLASS,
          "else" => LexemeKind::ELSE,
          "false" => LexemeKind::FALSE,
          "for" => LexemeKind::FOR,
          "fun" => LexemeKind::FUN,
          "if" => LexemeKind::IF,
          "nil" => LexemeKind::NIL,
          "or" => LexemeKind::OR,
          "print" => LexemeKind::PRINT,
          "return" => LexemeKind::RETURN,
          "super" => LexemeKind::SUPER,
          "this" => LexemeKind::THIS,
          "true" => LexemeKind::TRUE,
          "var" => LexemeKind::VAR,
          "while" => LexemeKind::WHILE,
          _ => LexemeKind::IDENTIFIER(buffer),
        }
    }
}

impl Iterator for Scanner {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_finished() {
            return tokenize(LexemeKind::EOF);
        }

        let c = self.chars[self.cursor];

        if is_number(c) {
            let num = self.number_boundary();
            return Some(Token { lexeme: LexemeKind::NUMBER(num) });
        } else if is_valid_ident(c) {
            let lexeme = self.identifier_boundary();
            return Some(Token { lexeme });
        }

        let lexeme = match c {
            '(' => tokenize(LexemeKind::LEFT_PAREN),
            ')' => tokenize(LexemeKind::RIGHT_PAREN),
            '{' => tokenize(LexemeKind::LEFT_BRACE),
            '}' => tokenize(LexemeKind::RIGHT_BRACE),
            ',' => tokenize(LexemeKind::COMMA),
            '.' => tokenize(LexemeKind::DOT),
            '-' => tokenize(LexemeKind::MINUS),
            '+' => tokenize(LexemeKind::PLUS),
            ';' => tokenize(LexemeKind::SEMICOLON),
            '*' => tokenize(LexemeKind::STAR),
            '!' => {
                let next = self.peek_next();
                tokenize(
                    if next == Some(&'=') {
                        self.advance();
                        LexemeKind::BANG_EQUAL
                    } else {
                        LexemeKind::BANG
                    }
                )
            }
            '=' => {
                let next = self.peek_next();
                tokenize(
                    if next == Some(&'=') {
                        self.advance();
                        LexemeKind::EQUAL_EQUAL
                    } else {
                        LexemeKind::EQUAL
                    }
                )
            }
            '<' => {
                let next = self.peek_next();
                tokenize(
                    if next == Some(&'=') {
                        self.advance();
                        LexemeKind::LESS_EQUAL
                    } else {
                        LexemeKind::LESS
                    }
                 )
            }
            '>' => {
                let next = self.peek_next();
                tokenize(
                    if next == Some(&'=') {
                        self.advance();
                        LexemeKind::GREATER_EQUAL
                    } else {
                        LexemeKind::GREATER
                    }
                )
            }
            '/' => {
                let next = self.peek_next();
                if next == Some(&'/') {
                    self.advance();
                    let mut done = false;
                    while !done {
                        if self.is_finished() {
                            done = true;
                        } else {
                            let next = self.peek_next();
                            if next != Some(&'\n') {
                                if self.is_finished() {
                                    done = true;
                                } else {
                                    self.advance();
                                }
                            } else {
                                done = true;
                            }
                        }
                    }

                    self.next()
                } else {
                    tokenize(LexemeKind::SLASH)
                }
            }
            c if c.is_whitespace() => {
                self.advance();
                self.next()
            }
            '"' => {
                let word = self.word_boundary();
                Some(Token { lexeme: LexemeKind::STRING(word) })
            }
            _ => tokenize(LexemeKind::EOF)
        };

        self.advance();
        lexeme
    }
}

fn is_number(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_valid_ident(c: char) -> bool {
    let re = Regex::new(r"[a-zA-Z_]").unwrap();
    re.is_match(&c.to_string())
}

fn tokenize(lexeme: LexemeKind) -> Option<Token> {
    Some(Token { lexeme })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut sc = Scanner::new("(!=) ==".to_owned());
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::LEFT_PAREN });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::BANG_EQUAL });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::RIGHT_PAREN });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EQUAL_EQUAL });
    }

    #[test]
    fn it_handles_comments() {
        let mut sc = Scanner::new("{} // foo".to_owned());
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::LEFT_BRACE });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::RIGHT_BRACE });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EOF });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EOF });
    }

    #[test]
    fn it_handles_comments_end() {
        let mut sc = Scanner::new("{} //".to_owned());
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::LEFT_BRACE });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::RIGHT_BRACE });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EOF });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EOF });
    }

    #[test]
    fn it_handles_strings() {
        let mut sc = Scanner::new("\"bar\" ".to_owned());
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::STRING("bar".to_string()) });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EOF });
    }

    #[test]
    fn it_handles_combo_strings() {
        let mut sc = Scanner::new("\"foo\" = \"bar\" ".to_owned());
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::STRING("foo".to_string()) });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EQUAL });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::STRING("bar".to_string()) });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EOF });
    }

    #[test]
    fn it_handles_numbers() {
        let mut sc = Scanner::new("1.2".to_owned());
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::NUMBER("1.2".parse().unwrap()) });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EOF });
    }

    #[test]
    fn it_handles_addition() {
        let mut sc = Scanner::new("1+2".to_owned());
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::NUMBER("1.0".parse().unwrap()) });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::PLUS });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::NUMBER("2.0".parse().unwrap()) });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EOF });
    }

    #[test]
    fn it_handles_reserved_words() {
        let mut sc = Scanner::new("and".to_owned());
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::AND });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EOF });
    }

    #[test]
    fn it_handles_idents_partial_reserved() {
        let mut sc = Scanner::new("andd".to_owned());
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::IDENTIFIER("andd".to_string()) });
        assert_eq!(sc.next().unwrap(), Token { lexeme: LexemeKind::EOF });
    }
}
