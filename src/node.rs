use crate::token::Token;
use serde::Serialize;


#[derive(Debug, Clone, Serialize)]
pub enum NFNode<'a> {
    Terminal(&'a Token, Annotation),
    NonTerminal(String, Annotation),
    Multiple(Vec<NFNode<'a>>, Annotation),
    Fmt(String),
    Epsilon,
    Unknown,
}

impl NFNode<'_> {
    pub fn order<'a>(node: &'a NFNode<'a>) -> Vec<&'a NFNode<'a>> {
        let mut result = Vec::new();
        Self::dfs(node, &mut result);
        result
    }

    fn dfs<'a>(node: &'a NFNode<'a>, result: &mut Vec<&'a NFNode<'a>>) {
        result.push(node);
        match node {
            NFNode::Terminal(_, _) => {}
            NFNode::NonTerminal(_, _) => {}
            NFNode::Multiple(children, _) => {
                for child in children {
                    Self::dfs(child, result);
                }
            }
            NFNode::Fmt(_) => {}
            NFNode::Epsilon => {}
            NFNode::Unknown => {}
        }
    }
}


#[derive(Debug, Clone, Serialize)]
pub enum EBNFTokenNode<'a> {
    Terminal(&'a Token, Annotation),
    NonTerminal(String, Annotation),
    Multiple(Vec<EBNFTokenNode<'a>>, Annotation),
    RegexExt(Box<EBNFTokenNode<'a>>, RegexExtKind, Annotation),
    Alternation(Box<EBNFTokenNode<'a>>, Box<EBNFTokenNode<'a>>, Annotation),
    Group(Box<EBNFTokenNode<'a>>, Annotation),
    Epsilon,
    Fmt(String),
    Unknown,
}

impl EBNFTokenNode<'_> {
    pub fn order<'a>(node: &'a EBNFTokenNode<'a>) -> Vec<&'a EBNFTokenNode<'a>> {
        let mut result = Vec::new();
        Self::dfs(node, &mut result);
        result
    }

    fn dfs<'a>(node: &'a EBNFTokenNode<'a>, result: &mut Vec<&'a EBNFTokenNode<'a>>) {
        result.push(node);
            match node {
                EBNFTokenNode::Alternation(a, b, _) => {
                    Self::dfs(a, result);
                    Self::dfs(b, result);
                }
                EBNFTokenNode::Multiple(a, _) => {
                    for k in a {
                        Self::dfs(k, result);
                    }
                }
                EBNFTokenNode::Group(a, _) => 
                    {
                    Self::dfs(a, result);
                    },
                EBNFTokenNode::RegexExt(a, _, _) => 
                    {
                    Self::dfs(a, result);
                    },
                _ => (),
            }
    }
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

impl EBNFNode {
    pub fn order(node: & EBNFNode) -> Vec<& EBNFNode> {
        let mut result = Vec::new();
        Self::dfs(node, &mut result);
        result
    }

    fn dfs<'a>(node: &'a EBNFNode, result: &mut Vec<&'a EBNFNode>) {
        result.push(node);
            match node {
                EBNFNode::Alternation(a, b, _) => {
                    Self::dfs(a, result);
                    Self::dfs(b, result);
                }
                EBNFNode::Multiple(a, _) => {
                    for k in a {
                        Self::dfs(k, result);
                    }
                }
                EBNFNode::Group(a, _) => 
                    {
                    Self::dfs(a, result);
                    },
                EBNFNode::RegexExt(a, _, _) => 
                    {
                    Self::dfs(a, result);
                    },
                _ => (),
            }
    }
}

#[derive(Debug, Clone, Serialize, Hash, PartialEq)]
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
