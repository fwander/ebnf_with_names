use std::collections::HashSet;
use std::hash::Hash;

use crate::Annotation;
use crate::EBNFNode;
use crate::Expression;
use crate::Grammar;
use crate::node::EBNFTokenNode;
use regex::Regex;
use serde::Serialize;
use serde::ser::SerializeStruct;

#[derive(Debug, Clone)]
pub struct Token {
    pub regex: Regex,
    pub annotation: Annotation,
}

impl Ord for Token {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        return self.regex.as_str().cmp(other.regex.as_str());
    }
}

impl PartialOrd for Token {
    fn lt(&self, other: &Self) -> bool {
        return self.regex.as_str().lt(&other.regex.as_str());
    }

    fn le(&self, other: &Self) -> bool {
        return self.regex.as_str().le(&other.regex.as_str());
    }

    fn gt(&self, other: &Self) -> bool {
        return self.regex.as_str().gt(&other.regex.as_str());
    }

    fn ge(&self, other: &Self) -> bool {
        return self.regex.as_str().ge(&other.regex.as_str());
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        return self.regex.as_str().partial_cmp(&other.regex.as_str());
    }
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
        state.serialize_field("annotation",&self.annotation)?;
        state.end()
    }
}

pub fn get_tokens(grammar: &Grammar<EBNFTokenNode>) -> HashSet<&Token> {
    let mut ret = HashSet::new();
    for e in &grammar.expressions {
        let order = EBNFTokenNode::order(&e.rhs);
        for n in order {
            if let EBNFTokenNode::Terminal(t,_) = n{
                ret.insert(t);
            }
        }
    };
    ret
}

pub fn token_pass(grammar: &mut Grammar<EBNFNode>) -> Grammar<EBNFTokenNode>{
    Grammar {
        expressions: grammar.expressions
            .iter()
            .map(|n| to_token_rule(n))
            .collect::<Vec<Expression<EBNFTokenNode>>>(),
    }
}

fn to_token_rule(rule: &Expression<EBNFNode>) -> Expression<EBNFTokenNode> {
    Expression {
        lhs: rule.lhs.to_owned(),
        rhs: to_token_node(&rule.rhs),
    }
}

fn to_token_node<'a>(node: &EBNFNode) -> EBNFTokenNode {
    match node {
        EBNFNode::Fmt(a) => EBNFTokenNode::Fmt(a.to_owned()),
        EBNFNode::Multiple(a, b) => {
            EBNFTokenNode::Multiple(a.into_iter().map(|o| to_token_node(o)).collect(), b.to_owned())
        }
        EBNFNode::NonTerminal(a, b) => EBNFTokenNode::NonTerminal(a.to_string(), b.to_owned()),
        EBNFNode::RegexString(s, a) => {
            let adding = Token {
                regex: Regex::new(&s).unwrap(),
                annotation: a.to_owned(),
            };
            EBNFTokenNode::Terminal(adding, a.to_owned())
        },
        EBNFNode::Terminal(s, a) => {
            let adding = Token {
                regex: Regex::new(&escape(&s)).unwrap(),
                annotation: a.to_owned(),
            };
            EBNFTokenNode::Terminal(adding, a.to_owned())
        },
        EBNFNode::Alternation(l, r, a) => EBNFTokenNode::Alternation(Box::new(to_token_node(l)), Box::new(to_token_node(r)), a.to_owned()),
        EBNFNode::Epsilon => EBNFTokenNode::Epsilon,
        EBNFNode::RegexExt(n, k, a) => EBNFTokenNode::RegexExt(Box::new(to_token_node(n)), k.to_owned(), a.to_owned()),
        EBNFNode::Group(n, a) => EBNFTokenNode::Group(Box::new(to_token_node(n)), a.to_owned()),
        EBNFNode::Unknown => todo!(),
    }
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

#[cfg(test)]
mod test {
    use std::result;

    use crate::{get_grammar};

    use insta::assert_yaml_snapshot;

    use super::Token;
    use super::get_tokens;
    use super::token_pass;
    use super::escape;

    
    #[test]
    fn escape_simple() {
        let source = "asdf".to_string();
        let result = escape(&source);
        assert_yaml_snapshot!(result)
    }

    #[test]
    fn escape_paren() {
        let source = ")".to_string();
        let result = escape(&source);
        assert_yaml_snapshot!(result)
    }
    
    #[test]
    fn simple_token() {
        let source = r"
         filter ::= a | b;
         a ::= 'a';
         b ::= 'b';
     ";
        let result = token_pass(&mut get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    
    #[test]
    fn dup_token() {
        let source = r"
         filter ::= a | b;
         a ::= 'a';
         b ::= 'a';
     ";
        let result = token_pass(&mut get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }

    #[test]
    fn get_simple_token() {
        let source = r"
         filter ::= a | b;
         a ::= 'a';
         b ::= 'b';
     ";
        let binding = token_pass(&mut get_grammar(source).unwrap());
        let mut result = get_tokens(&binding).into_iter().collect::<Vec<&Token>>();
        result.sort();
        assert_yaml_snapshot!(result )
    }

    #[test]
    fn get_dup_token() {
        let source = r"
         filter ::= a | b;
         a ::= 'a';
         b ::= 'a';
     ";
        let binding = token_pass(&mut get_grammar(source).unwrap());
        let mut result = get_tokens(&binding).into_iter().collect::<Vec<&Token>>();
        result.sort();
        assert_yaml_snapshot!(result )
    }
}