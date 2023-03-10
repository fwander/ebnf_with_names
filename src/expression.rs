use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct Expression<T> {
    pub lhs: String,
    pub rhs: T,
}
