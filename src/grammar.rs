use crate::expression::Expression;
use crate::token::Token;
use serde::Serialize;
#[derive(Debug, Clone, Serialize)]
pub struct Grammar<T> {
    pub expressions: Vec<Expression<T>>,
    pub tokens: Vec<Token>,
}
