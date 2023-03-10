use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub enum NFNode {
    Terminal(String, Annotation),
    RegexString(String, Annotation),
    NonTerminal(String, Annotation),
    Multiple(Box<NFNode>, Box<NFNode>, Annotation),
    Fmt(String),
    Epsilon,
    Unknown,
}

#[derive(Debug, Clone, Serialize)]
pub enum EBNFNode {
    Terminal(String, Annotation),
    RegexString(String, Annotation),
    NonTerminal(String, Annotation),
    Multiple(Vec<EBNFNode>, Annotation),
    RegexExt(Box<EBNFNode>, RegexExtKind, Annotation),
    Alternation(Box<EBNFNode>, Box<EBNFNode>, Annotation),
    Group(Box<EBNFNode>, Annotation),
    Epsilon,
    Fmt(String),
    Unknown,
}

#[derive(Debug, Clone, Serialize)]
pub struct Annotation {
    pub name: Option<String>,
}

#[derive(Debug, Clone, Serialize)]
pub enum RegexExtKind {
    Repeat0,
    Repeat1,
    Optional,
}

pub enum DeriveKind {}
