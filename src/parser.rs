use crate::lexer::{Token, LexemeKind};

enum Expr {
    Binary(ExprBinary),
    Literal(ExprLiteral),
    Unary(ExprUnary),
}

struct ExprBinary {
    left: Box<ExprBinary>,
    operator: Token,
    right: Box<ExprBinary>
}

struct ExprUnary {
    operator: LexemeKind,
    right: Box<Expr>
}

enum ExprLiteral {
    BOOLEAN(bool),
    STRING(String),
    NUMBER(i32),
}

pub struct Parser {
    tokens: Vec<Token>,
    cursor: usize,
    binary: Option<ExprBinary>
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            cursor: 0,
            binary: None
        }
    }

    pub fn parse(&mut self) -> Expr {
        self.expression()
    }

    fn push_event(&mut self, left: ExprBinary, operator: Token, right: ExprBinary) {
        self.binary = Some(ExprBinary {
            left: Box::new(left),
            operator,
            right: Box::new(right)
        })
    }

    fn at_end(&self) -> bool {
        self.peek_kind() == Some(&LexemeKind::EOF)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.cursor)
    }

    fn peek_previous(&self) -> Option<&Token> {
        self.tokens.get(self.cursor - 1)
    }

    fn peek_kind(&self) -> Option<&LexemeKind> {
        self.peek().map(|Token { lexeme, .. }| lexeme)
    }

    fn peek_previous_kind(&self) -> Option<&LexemeKind> {
        self.peek_previous().map(|Token { lexeme, .. }| lexeme)
    }

    fn consume() {
        todo!();
    }

    fn advance(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.cursor);

        self.cursor += 1;

        token
    }

    fn at(&self, kind: LexemeKind) -> bool {
        if self.at_end() { return false };
        self.peek_kind() == Some(&kind)
    }

    fn is_equal(&self, kinds: Vec<LexemeKind>) -> bool {
        let res = kinds.iter().find(|&&k| self.at(k));
        res.is_some()
    }

    fn expression(&mut self) -> Expr {
       self.equality()
    }

    fn equality(&mut self) -> Expr {
        let expr = self.comparison();

        while self.is_equal(vec![LexemeKind::BANG_EQUAL, LexemeKind::EQUAL_EQUAL]) {
            todo!();
        }

        expr
    }

    fn comparison(&mut self) -> Expr {
        let expr = self.term();

        while self.is_equal(vec![LexemeKind::GREATER, LexemeKind::GREATER_EQUAL, LexemeKind::LESS, LexemeKind::LESS_EQUAL]) {
            todo!();
        }

        expr
    }

    fn term(&mut self) -> Expr {
        let expr = self.factor();

        while self.is_equal(vec![LexemeKind::MINUS, LexemeKind::PLUS]) {
            todo!();
        }

        expr
    }

    fn factor(&mut self) -> Expr {
        let expr = self.unary();

        while self.is_equal(vec![LexemeKind::SLASH, LexemeKind::STAR]) {
            todo!();
        }

        expr
    }

    fn unary(&mut self) -> Expr {
        let res = None;
        while self.is_equal(vec![LexemeKind::BANG, LexemeKind::MINUS]) {
            let operator = self.peek_previous_kind().unwrap();
            let right = self.unary();
            res = Some(Expr::Unary(ExprUnary { operator: *operator, right: Box::new(right) }))
        }

        if res.is_some() { res.unwrap() } else { self.primary() }
    }

    fn primary(&mut self) -> Expr {
        if self.at(LexemeKind::FALSE) {
            let val = self.advance();
            Expr::Literal(ExprLiteral::BOOLEAN(false))
        } else if self.at(LexemeKind::TRUE) {
            let val = self.advance();
            Expr::Literal(ExprLiteral::BOOLEAN(true))
        } else if self.at(LexemeKind::STRING(String)) {
            let val = self.advance();
            val.map(|Token { &LexemeKind(*st), .. }| Expr::Literal(ExprLiteral::STRING(st)))
        } else if self.at(LexemeKind::NUMBER(_)) {
            let val = self.advance();
            val.map(|Token { &LexemeKind(*num), .. }| Expr::Literal(ExprLiteral::NUMBER(num))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_works() {
        let tokens = Scanner::new("(!=) ==".to_owned()).collect();
        let ast = Parser::new(tokens).parse();
    }
}
