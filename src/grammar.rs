use crate::expression::Expression;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct Grammar<T> {
    pub expressions: Vec<Expression<T>>,
}
