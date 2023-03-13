use crate::token::Token;
use serde::Serialize;


#[derive(Debug, Clone, Serialize)]
pub enum NFNode {
    Terminal(Token, Annotation),
    NonTerminal(String, Annotation),
    Multiple(Vec<NFNode>, Annotation),
    Fmt(String),
    Epsilon,
    Unknown,
}

impl NFNode {
    pub fn order(node: &NFNode) -> Vec<&NFNode> {
        let mut result = Vec::new();
        Self::dfs(node, &mut result);
        result
    }

    fn dfs<'a>(node: &'a NFNode, result: &mut Vec<&'a NFNode>) {
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
pub enum EBNFTokenNode {
    Terminal(Token, Annotation),
    NonTerminal(String, Annotation),
    Multiple(Vec<EBNFTokenNode>, Annotation),
    RegexExt(Box<EBNFTokenNode>, RegexExtKind, Annotation),
    Alternation(Box<EBNFTokenNode>, Box<EBNFTokenNode>, Annotation),
    Group(Box<EBNFTokenNode>, Annotation),
    Epsilon,
    Fmt(String),
    Unknown,
}

impl EBNFTokenNode {
    pub fn order(node: &EBNFTokenNode) -> Vec<&EBNFTokenNode> {
        let mut result = Vec::new();
        Self::dfs(node, &mut result);
        result
    }

    fn dfs<'a>(node: &'a EBNFTokenNode, result: &mut Vec<&'a EBNFTokenNode>) {
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
    pub fn walk<F>(&mut self, down: Option<F>, up: Option<F>) -> &mut EBNFNode
    where F: Fn(&mut EBNFNode) -> &mut EBNFNode + Copy
    {
        if let Some(d) = down{
            d(self);
        }
        match self {
            EBNFNode::Alternation(a, b, _) => {
                a.walk(down, up);
                b.walk(down, up);
            }
            EBNFNode::Multiple(a, _) => {
                for k in a {
                    k.walk(down, up);
                }
            }
            EBNFNode::Group(a, _) => 
                {
                a.walk(down, up);
                },
            EBNFNode::RegexExt(a, _, _) => 
                {
                a.walk(down, up);
                },
            _ => (),
            }
        if let Some(u) = up{
            u(self)
        }
        else {
            self
        }
    }
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
