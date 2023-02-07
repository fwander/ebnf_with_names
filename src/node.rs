use serde::{Serialize};

#[derive(Debug, Clone, Serialize)]
pub enum Node {
    String(String, String),
    RegexString(String, String),
    Terminal(String, String),
    Multiple(Vec<Node>),
    RegexExt(Box<Node>, RegexExtKind, String),
    Symbol(Box<Node>, SymbolKind, Box<Node>),
    Group(Box<Node>, String),
    Optional(Box<Node>, String),
    Repeat(Box<Node>, String),
    Fmt(String),
    Unknown,
}

#[derive(Debug, Clone, Serialize)]
pub enum RegexExtKind {
    Repeat0,
    Repeat1,
    Optional
}

#[derive(Debug, Clone, Serialize)]
pub enum SymbolKind {
    Concatenation,
    Alternation,
}