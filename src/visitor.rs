use crate::lexer::{LexemeKind, Token};
use crate::parser::{Expr, Stmt, Value};

// Dynamic dispatch
// This has a higher runtime cost due to vtable lookups.
// This is compared to static dispatch, which will monomorphize each function that expects a
// generic type T.
// Everything is behind a  reference because we pass around
pub trait ExpressionVisitor<T> {
    fn visit_assign(&mut self, name: &str, expr: &Expr) -> T;
    fn visit_binary(&mut self, left: &Expr, operator: &LexemeKind, right: &Expr) -> T;
    fn visit_call(&mut self, callee: &Expr, arguments: &Vec<Expr>) -> T;
    fn visit_logical(&mut self, left: &Expr, operator: &LexemeKind, right: &Expr) -> T;
    fn visit_literal(&mut self, val: &Value) -> T;
    fn visit_unary(&mut self, operator: &LexemeKind, right: &Expr) -> T;
    fn visit_grouping(&mut self, val: &Expr) -> T;
    fn visit_variable(&mut self, ident: &str) -> T;
    fn visit_error(&mut self, line: &usize, message: &str) -> T;
}

// "a statement...will not return a value". It means that a statement is not processed by evaluating
// it into a final value (as an expression is), but is rather processed by executing it
pub trait StatementVisitor<T> {
    fn visit_block(&mut self, stmts: &Vec<Stmt>) -> T;
    fn visit_if(&mut self, condition: &Expr, then_branch: &Stmt, else_branch: &Option<Stmt>) -> T;
    fn visit_func_declaration(&mut self, ident: &str, parameters: &Vec<Token>, body: &Box<Stmt>) -> T;
    fn visit_while(&mut self, condition: &Expr, body: &Stmt) -> T;
    fn visit_variable_def(&mut self, ident: &str, expr: &Option<Expr>) -> T;
    fn visit_print(&mut self, expr: &Option<Expr>) -> T;
    fn visit_expr(&mut self, expr: &Expr) -> T;
    fn visit_error(&mut self, line: &usize, message: &str) -> T;
    fn visit_return(&mut self, expression: &Option<Expr>) -> T;
}
