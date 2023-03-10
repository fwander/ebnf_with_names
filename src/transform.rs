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
        EBNFNode::Multiple(a, b) => NFNode::Multiple(
            Box::new(to_nf_node(a[0].to_owned())),
            Box::new(to_nf_node(a[1].to_owned())),
            b,
        ),
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
        EBNFNode::Multiple(vec, annotation) => {
            if vec.len() == 0 {
                todo!();
            } else if vec.len() == 1 {
                new_exprs.push(Expression {
                    lhs: rule.lhs.to_owned(),
                    rhs: vec[0].to_owned(),
                });
                any = true;
            } else if vec.len() == 2 {
                let mut was_term = false;
                match &vec[0] {
                    EBNFNode::NonTerminal(_, _) => {}
                    _ => {
                        let new_nonterm = format!("~{}", uniqueifier);
                        *uniqueifier = *uniqueifier + 1;
                        new_exprs.push(Expression {
                            lhs: new_nonterm.to_owned(),
                            rhs: vec[0].to_owned(),
                        });
                        new_exprs.push(Expression {
                            lhs: rule.lhs.to_owned(),
                            rhs: EBNFNode::Multiple(
                                vec![
                                    EBNFNode::NonTerminal(new_nonterm, Annotation { name: None }),
                                    vec[1].to_owned(),
                                ],
                                annotation.to_owned(),
                            ),
                        });
                        was_term = true;
                    }
                }

                if !was_term { //first element of two was not a terminal
                    match &vec[1] {
                        EBNFNode::NonTerminal(_, _) => {}
                        _ => {
                            let new_nonterm = format!("~{}", uniqueifier);
                            *uniqueifier = *uniqueifier + 1;
                            new_exprs.push(Expression {
                                lhs: new_nonterm.to_owned(),
                                rhs: vec[1].to_owned(),
                            });
                            new_exprs.push(Expression {
                                lhs: rule.lhs.to_owned(),
                                rhs: EBNFNode::Multiple(
                                    vec![
                                        vec[0].to_owned(),
                                        EBNFNode::NonTerminal(
                                            new_nonterm,
                                            Annotation { name: None },
                                        ),
                                    ],
                                    annotation.to_owned(),
                                ),
                            });
                            was_term = true;
                        }
                    }
                }

                if was_term {
                    any = was_term;
                } else {
                    new_exprs.push(Expression {
                        lhs: rule.lhs.to_owned(),
                        rhs: EBNFNode::Multiple(vec, annotation),
                    });
                }
            } else { // list has more than 2 elements, we need to reduce
                any = true;
                // E <- A B C...
                // E <- A [K]
                // [K] <- B C...
                let new_vec = vec[1..].to_vec();
                let new_nonterm = format!("~{}", uniqueifier);
                *uniqueifier = *uniqueifier + 1;
                new_exprs.push(Expression {
                    lhs: new_nonterm.to_owned(),
                    rhs: EBNFNode::Multiple(new_vec, annotation.clone()),
                });
                new_exprs.push(Expression {
                    lhs: rule.lhs.to_owned(),
                    rhs: EBNFNode::Multiple((&[vec[0].to_owned(),EBNFNode::NonTerminal(new_nonterm.to_owned(), Annotation {name : None})]).to_vec(), Annotation { name: None }),
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
}
