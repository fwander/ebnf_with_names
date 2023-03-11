use std::mem;

use crate::{Annotation, EBNFNode, Expression, Grammar, NFNode, RegexExtKind};

fn to_nf_grammar(input: Grammar<EBNFNode>) -> Grammar<NFNode> {
    let mut expr_vec: Vec<Expression<EBNFNode>> = input.expressions;
    let mut new_expr_vec: Vec<Expression<EBNFNode>> = Vec::new();
    let mut uniqueifier = 0;
    let mut any = true;
    while any {
        any = false;
        for rule in expr_vec {
            let (mut resp, cont) = expand_rule(rule, &mut uniqueifier);
            new_expr_vec.append(&mut resp);
            any = any || cont;
        }
        expr_vec = new_expr_vec;
        new_expr_vec = Vec::new();
    }

    Grammar {
        expressions: expr_vec
            .into_iter()
            .map(|n| to_nf_rule(n))
            .collect::<Vec<Expression<NFNode>>>(),
    }
}

fn to_nf_rule(rule: Expression<EBNFNode>) -> Expression<NFNode> {
    Expression {
        lhs: rule.lhs.to_owned(),
        rhs: to_nf_node(rule.rhs),
    }
}

//multiple nodes should only contain two nodes
fn to_nf_node(node: EBNFNode) -> NFNode {
    match node {
        EBNFNode::Fmt(a) => NFNode::Fmt(a),
        EBNFNode::Multiple(a, b) => NFNode::Multiple( a,b),
        EBNFNode::NonTerminal(a, b) => NFNode::NonTerminal(a, b),
        EBNFNode::RegexString(a, b) => NFNode::RegexString(a, b),
        EBNFNode::Terminal(a, b) => NFNode::Terminal(a, b),
        EBNFNode::Epsilon => NFNode::Epsilon,
        _ => unimplemented!(),
    }
}

//performs one step of the ebnf to bnf reduction
fn expand_rule(
    rule: Expression<EBNFNode>,
    uniqueifier: &mut i32,
) -> (Vec<Expression<EBNFNode>>, bool) {
    let mut any = false;
    let mut new_exprs: Vec<Expression<EBNFNode>> = Vec::new();
    match rule.rhs {
        EBNFNode::Terminal(_, _)
        | EBNFNode::RegexString(_, _)
        | EBNFNode::NonTerminal(_, _)
        | EBNFNode::Epsilon => {
            new_exprs.push(rule);
        }
        EBNFNode::Multiple(mut vec, annotation) => {
            if vec.len() == 0 {
                unimplemented!();
            } else if vec.len() == 1 {
                new_exprs.push(Expression {
                    lhs: rule.lhs.to_owned(),
                    rhs: vec[0].to_owned(),
                });
                any = true;
            } else {
                for i in 0..vec.len() {
                    match vec[i] {
                        EBNFNode::NonTerminal(_, _) => {}
                        _ => {
                            any = true;
                            let new_nonterm = format!("~{}", uniqueifier);
                            *uniqueifier = *uniqueifier + 1;
                            vec.push(EBNFNode::NonTerminal(new_nonterm.clone(), annotation.clone()));
                            new_exprs.push(Expression {
                                lhs: new_nonterm.to_owned(),
                                rhs: vec.swap_remove(i),
                            });
                        }
                    }
                }
                new_exprs.push(Expression {
                    lhs: rule.lhs.to_owned(),
                    rhs: EBNFNode::Multiple(
                        vec,
                        annotation.clone(),
                    ),
                });
            }
        }
        EBNFNode::RegexExt(b, kind, annotation) => {
            any = true;
            match kind {
                // E <- B?
                // --->
                // E <- B
                // E <- \epsilon
                RegexExtKind::Optional => {
                    new_exprs.push(Expression {
                        lhs: rule.lhs.to_owned(),
                        rhs: *b
                    });
                    new_exprs.push(Expression {
                        lhs: rule.lhs.to_owned(),
                        rhs: EBNFNode::Epsilon,
                    });
                }
                ,
                // E <- B*
                // --->
                // E <- [K]
                // [k] <- B[k]
                // [k] <- \epsilon
                RegexExtKind::Repeat0 => {
                    let new_nonterm = format!("~{}", uniqueifier);
                    *uniqueifier = *uniqueifier + 1;
                    new_exprs.push(Expression {
                        lhs: rule.lhs.to_owned(),
                        rhs: EBNFNode::NonTerminal(new_nonterm.to_owned(), annotation.clone()),
                    });
                    new_exprs.push(Expression {
                        lhs: new_nonterm.to_owned(),
                        rhs: EBNFNode::Multiple(
                            vec![
                                *b,
                                EBNFNode::NonTerminal(new_nonterm.to_owned(), annotation.clone()),
                            ],
                            Annotation { name: None },
                        ),
                    });
                    new_exprs.push(Expression {
                        lhs: new_nonterm.to_owned(),
                        rhs: EBNFNode::Epsilon,
                    });
                } ,
                // E <- B+
                // --->
                // E <- [K]
                // [k] <- B[k]
                // [k] <- B
                RegexExtKind::Repeat1 => {
                    let new_nonterm = format!("~{}", uniqueifier);
                    *uniqueifier = *uniqueifier + 1;
                    new_exprs.push(Expression {
                        lhs: rule.lhs.to_owned(),
                        rhs: EBNFNode::NonTerminal(new_nonterm.to_owned(), annotation.clone()),
                    });
                    new_exprs.push(Expression {
                        lhs: new_nonterm.to_owned(),
                        rhs: EBNFNode::Multiple(
                            vec![
                                (*b).clone(),
                                EBNFNode::NonTerminal(new_nonterm.to_owned(), annotation.clone()),
                            ],
                            Annotation { name: None },
                        ),
                    });
                    new_exprs.push(Expression {
                        lhs: new_nonterm.to_owned(),
                        rhs: *b,
                    });
                }
            }
        }
        EBNFNode::Alternation(a, b, c) => {
            //newExprs.push(Expression<EBNFNode>::new() a);
            new_exprs.push(Expression {
                lhs: rule.lhs.to_owned(),
                rhs: *a,
            });
            //*uniqueifier = *uniqueifier + 1;
            new_exprs.push(Expression {
                lhs: rule.lhs.to_owned(),
                rhs: *b,
            });
            //*uniqueifier = *uniqueifier + 1;
            any = true;
        }
        EBNFNode::Group(a, _) => {
            new_exprs.push(Expression {
                lhs: rule.lhs.to_owned(),
                rhs: *a,
            });
            any = true;
        }
        EBNFNode::Fmt(_) => {
            new_exprs.push(rule);
        },
        EBNFNode::Unknown => todo!(),
    }
    return (new_exprs, any);
}

#[cfg(test)]
mod test {
    use crate::get_grammar;

    use super::*;
    use insta::assert_yaml_snapshot;
    #[test]
    fn no_change() {
        let source = r"
         A ::= a;
         B ::= b;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn no_change_2() {
        let source = r"
         A ::= a;
         A ::= b;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn alternation_removal() {
        let source = r"
         A ::= a | b;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn alternation_removal_3() {
        let source = r"
         A ::= a | b | c;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn alternation_removal_non_term() {
        let source = r"
         A ::= B | C;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn group_removal() {
        let source = r"
         A ::= (a);
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn multiple_no_change() {
        let source = r"
         A ::= A B;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn multiple_3() {
        let source = r"
         A ::= A B C;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn regex_plus() {
        let source = r"
         A ::= A+;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn regex_star() {
        let source = r"
         A ::= A*;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn regex_question() {
        let source = r"
         A ::= A?;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn regex_plus_multiple() {
        let source = r"
         A ::= A B+;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn regex_plus_multiple_3() {
        let source = r"
         A ::= A B C+;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    //
    //
    //
    //nesting
    #[test]
    fn sub_alternation_removal() {
        let source = r"
         A ::= A | (B | c);
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn sub_alternation_removal_multiple() {
        let source = r"
         A ::= A (B | c);
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn sub_alternation_removal_multiple_2() {
        let source = r"
         A ::= A B (B | c);
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn sub_sub_alternation_removal() {
        let source = r"
         A ::= B | (C | (D | E));
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn sub_alternation_removal_sub_multiple() {
        let source = r"
         A ::= B | (C D);
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn sub_alternation_removal_sub_multiple_outer_multiple() {
        let source = r"
         A ::= (B | (C D)) E;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn alternation_star() {
        let source = r"
         A ::= (B | C)*;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn alternation_star_multiple() {
        let source = r"
         A ::= (B | C)* D;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn multiple_star() {
        let source = r"
         A ::= (B C)*;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
}
