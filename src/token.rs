use std::collections::HashSet;
use std::hash::Hash;

use crate::Annotation;
use crate::EBNFNode;
use crate::Grammar;
use regex::Regex;
use serde::Serialize;
use serde::ser::SerializeStruct;

#[derive(Debug, Clone)]
pub struct Token {
    pub regex: Regex,
    pub annotation: Annotation,
}

impl Eq for Token {
    fn assert_receiver_is_total_eq(&self) {}
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.regex.as_str() == other.regex.as_str() && self.annotation == other.annotation
    }
}

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.regex.as_str().hash(state);
        self.annotation.hash(state);
    }
}

impl Serialize for Token {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("Token", 2)?;
        state.serialize_field("regex",self.regex.as_str())?;
        state.serialize_field("regex",&self.annotation)?;
        state.end()
    }
}

pub fn token_pass(grammar: &mut Grammar<EBNFNode>) {
    let mut set: HashSet<Token> = HashSet::new();
}

fn extract_tokens(node: &EBNFNode, adding_to: &mut HashSet<Token>) {
    let operate = |on: &EBNFNode, adding_to: &mut HashSet<Token>| {
        match on {
            EBNFNode::RegexString(s, a) => {
                adding_to.insert(Token {
                    regex: Regex::new(s).unwrap(),
                    annotation: a.to_owned(),
                });
            }
            EBNFNode::Terminal(s, a) => {
                adding_to.insert(Token {
                    regex: Regex::new(&escape(s)).unwrap(),
                    annotation: a.to_owned(),
                });
            }
            _ => (),
        };
    };
    let vec = EBNFNode::order(node);
    vec.into_iter().map(|n| {operate(n, adding_to)});
}

fn escape(regex: &String) -> String {
    let mut result = "".to_string();

    for character in regex.chars() {
            match character {
                '\\' => result += "\\\\",
                '^' => result += "\\^",
                '$' => result += "\\$",
                '.' => result += "\\.",
                '|' => result += "\\|",
                '?' => result += "\\?",
                '*' => result += "\\*",
                '+' => result += "\\+",
                '(' => result += "\\(",
                ')' => result += "\\)",
                '[' => result += "\\[",
                ']' => result += "\\]",
                '{' => result += "\\{",
                '}' => result += "\\}",
                a => result.push(a),
            };
    }
    return result;
}
