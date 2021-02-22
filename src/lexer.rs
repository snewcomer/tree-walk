extern crate regex;
use regex::Regex;

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

    UNEXPECTED,

    EOF
}

#[derive(Debug, PartialEq)]
pub struct Token {
    line: usize,
    pub lexeme: LexemeKind,
}

impl Token {
    pub fn new(lexeme: LexemeKind, line: usize) -> Self {
        Self {
            lexeme,
            line
        }
    }
}

pub struct Scanner {
    cursor: usize,
    chars: Vec<char>,
    line: usize,
}

// Lexer - group raw substrings into lexemes.  This is a higher representation than the raw source.
impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            // time and space higher with collect
            chars: source.chars().collect(),
            cursor: 0,
            line: 0,
        }
    }

    fn current_char(&self) -> Option<&char> {
        self.chars.get(self.cursor)
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
            let c = self.current_char().unwrap();
            match *c {
                add if is_number(add) || add == '.' => {
                    buffer.push(add.to_owned());
                    self.cursor += 1;
                }
                _ => break,
            }
        }

        buffer.parse().unwrap()
    }

    fn word_boundary(&mut self) -> String {
        // first was ". next char is potentially the word
        self.cursor += 1;
        let mut buffer = String::new();
        while self.peek_next().is_some() {
            let c = self.current_char().unwrap();
            match *c {
                '"' => break,
                add => {
                    buffer.push(add.to_owned());
                    self.cursor += 1;
                }
                _ => break,
            }
        }

        buffer
    }

    fn identifier_boundary(&mut self) -> LexemeKind {
        let mut buffer = String::new();
        while self.current_char().is_some() {
            let c = self.current_char().unwrap();
            match *c {
                add if is_number(add) || is_valid_ident(add) => {
                    buffer.push(add.to_owned());
                    self.cursor += 1;
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
            return Some(Token::new(LexemeKind::EOF, self.line));
        }

        let c = self.chars[self.cursor];

        if is_number(c) {
            let num = self.number_boundary();
            return Some(Token::new(LexemeKind::NUMBER(num), self.line));
        } else if is_valid_ident(c) {
            let lexeme = self.identifier_boundary();
            return Some(Token::new(lexeme, self.line));
        }

        let lexeme = match c {
            '(' => Some(Token::new(LexemeKind::LEFT_PAREN, self.line)),
            ')' => Some(Token::new(LexemeKind::RIGHT_PAREN, self.line)),
            '{' => Some(Token::new(LexemeKind::LEFT_BRACE, self.line)),
            '}' => Some(Token::new(LexemeKind::RIGHT_BRACE, self.line)),
            ',' => Some(Token::new(LexemeKind::COMMA, self.line)),
            '.' => Some(Token::new(LexemeKind::DOT, self.line)),
            '-' => Some(Token::new(LexemeKind::MINUS, self.line)),
            '+' => Some(Token::new(LexemeKind::PLUS, self.line)),
            ';' => Some(Token::new(LexemeKind::SEMICOLON, self.line)),
            '*' => Some(Token::new(LexemeKind::STAR, self.line)),
            '!' => {
                let next = self.peek_next();
                Some(Token::new(
                    if next == Some(&'=') {
                        self.cursor += 1;
                        LexemeKind::BANG_EQUAL
                    } else {
                        LexemeKind::BANG
                    },
                    self.line
                ))
            }
            '=' => {
                let next = self.peek_next();
                Some(Token::new(
                    if next == Some(&'=') {
                        self.cursor += 1;
                        LexemeKind::EQUAL_EQUAL
                    } else {
                        LexemeKind::EQUAL
                    },
                    self.line
                ))
            }
            '<' => {
                let next = self.peek_next();
                Some(Token::new(
                    if next == Some(&'=') {
                        self.cursor += 1;
                        LexemeKind::LESS_EQUAL
                    } else {
                        LexemeKind::LESS
                    },
                    self.line
                 ))
            }
            '>' => {
                let next = self.peek_next();
                Some(Token::new(
                    if next == Some(&'=') {
                        self.cursor += 1;
                        LexemeKind::GREATER_EQUAL
                    } else {
                        LexemeKind::GREATER
                    },
                    self.line
                ))
            }
            '/' => {
                let next = self.peek_next();
                if next == Some(&'/') {
                    self.cursor += 1;
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
                                    self.cursor += 1;
                                }
                            } else {
                                done = true;
                            }
                        }
                    }

                    // We aren't capturing tokens because the point of this is to execute the
                    // program and not faithfully represent every character (lossless)
                    self.next()
                } else {
                    Some(Token::new(LexemeKind::SLASH, self.line))
                }
            }
            c if c.is_whitespace() => {
                if c == '\n' {
                    self.line += 1;
                }
                self.cursor += 1;
                self.next()
            }
            '"' => {
                let word = self.word_boundary();
                Some(Token::new(LexemeKind::STRING(word), self.line))
            }
            _ => {
                if self.is_finished() {
                    Some(Token::new(LexemeKind::EOF, self.line))
                } else {
                    // TODO: how to handle errors.  Should this only happen in the parse phase?
                    Some(Token::new(LexemeKind::UNEXPECTED, self.line))
                }
            }
        };

        self.cursor += 1;
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


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut sc = Scanner::new("(!=) ==".to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::LEFT_PAREN, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::BANG_EQUAL, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::RIGHT_PAREN, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EQUAL_EQUAL, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
    }

    #[test]
    fn it_handles_comments() {
        let mut sc = Scanner::new("{} // foo".to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::LEFT_BRACE, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::RIGHT_BRACE, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
    }

    #[test]
    fn it_handles_comments_end() {
        let mut sc = Scanner::new("{} //".to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::LEFT_BRACE, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::RIGHT_BRACE, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
    }

    #[test]
    fn it_handles_strings() {
        let mut sc = Scanner::new("\"bar\" ".to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::STRING("bar".to_string()), 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
    }

    #[test]
    fn it_handles_combo_strings() {
        let mut sc = Scanner::new("\"foo\" = \"bar\" ".to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::STRING("foo".to_string()), 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EQUAL, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::STRING("bar".to_string()), 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
    }

    #[test]
    fn it_handles_numbers() {
        let mut sc = Scanner::new("1.2".to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::NUMBER("1.2".parse().unwrap()), 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
    }

    #[test]
    fn it_handles_addition() {
        let mut sc = Scanner::new("1+2.0".to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::NUMBER("1.0".parse().unwrap()), 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::PLUS, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::NUMBER("2.0".parse().unwrap()), 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
    }

    #[test]
    fn it_handles_reserved_words() {
        let mut sc = Scanner::new("and".to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::AND, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));

        let mut sc = Scanner::new("while".to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::WHILE, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
    }

    #[test]
    fn it_handles_idents_partial_reserved() {
        let mut sc = Scanner::new("andd".to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::IDENTIFIER("andd".to_string()), 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
    }

    #[test]
    fn it_handles_newlines() {
        let source = "
and while

andd
";
        let mut sc = Scanner::new(source.to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::AND, 1));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::WHILE, 1));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::IDENTIFIER("andd".to_string()), 3));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 3));
    }

    #[test]
    fn it_handles_unexpected_character() {
        let source = "/·";
        let mut sc = Scanner::new(source.to_owned());
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::SLASH, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::UNEXPECTED, 0));
        assert_eq!(sc.next().unwrap(), Token::new(LexemeKind::EOF, 0));
    }
}
