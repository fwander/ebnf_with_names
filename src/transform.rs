use crate::{node::EBNFTokenNode, Annotation, EBNFNode, Expression, Grammar, NFNode, RegexExtKind};
use std::mem;

fn to_nf_grammar(input: Grammar<EBNFTokenNode>) -> Grammar<NFNode> {
    let mut expr_vec: Vec<Expression<EBNFTokenNode>> = input.expressions;
    let mut new_expr_vec: Vec<Expression<EBNFTokenNode>> = Vec::new();
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
        tokens: Vec::new(),
    }
}

fn to_nf_rule(rule: Expression<EBNFTokenNode>) -> Expression<NFNode> {
    Expression {
        lhs: rule.lhs.to_owned(),
        rhs: to_nf_node(rule.rhs),
    }
}

//multiple nodes should only contain two nodes
fn to_nf_node(node: EBNFTokenNode) -> NFNode {
    match node {
        EBNFTokenNode::Fmt(a) => NFNode::Fmt(a),
        EBNFTokenNode::Multiple(a, b) => {
            NFNode::Multiple(a.into_iter().map(|o| to_nf_node(o)).collect(), b)
        }
        EBNFTokenNode::NonTerminal(a, b) => NFNode::NonTerminal(a, b),
        EBNFTokenNode::Terminal(a, b) => NFNode::Terminal(a, b),
        EBNFTokenNode::Epsilon => NFNode::Epsilon,
        _ => unimplemented!(),
    }
}

//performs one step of the ebnf to bnf reduction
fn expand_rule<'a>(
    rule: Expression<EBNFTokenNode<'a>>,
    uniqueifier: &mut i32,
) -> (Vec<Expression<EBNFTokenNode<'a>>>, bool) {
    let mut any = false;
    let mut new_exprs: Vec<Expression<EBNFTokenNode>> = Vec::new();
    match rule.rhs {
        EBNFTokenNode::Terminal(_, _)
        | EBNFTokenNode::NonTerminal(_, _)
        | EBNFTokenNode::Epsilon => {
            new_exprs.push(rule);
        }
        EBNFTokenNode::Multiple(mut vec, annotation) => {
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
                        EBNFTokenNode::NonTerminal(_, _) => {}
                        _ => {
                            any = true;
                            let new_nonterm = format!("~{}", uniqueifier);
                            *uniqueifier = *uniqueifier + 1;
                            vec.push(EBNFTokenNode::NonTerminal(
                                new_nonterm.clone(),
                                annotation.clone(),
                            ));
                            new_exprs.push(Expression {
                                lhs: new_nonterm.to_owned(),
                                rhs: vec.swap_remove(i),
                            });
                        }
                    }
                }
                new_exprs.push(Expression {
                    lhs: rule.lhs.to_owned(),
                    rhs: EBNFTokenNode::Multiple(vec, annotation.clone()),
                });
            }
        }
        EBNFTokenNode::RegexExt(b, kind, annotation) => {
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
                        rhs: EBNFTokenNode::Epsilon,
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
                        rhs: EBNFTokenNode::NonTerminal(new_nonterm.to_owned(), annotation.clone()),
                    });
                    new_exprs.push(Expression {
                        lhs: new_nonterm.to_owned(),
                        rhs: EBNFTokenNode::Multiple(
                            vec![
                                *b,
                                EBNFTokenNode::NonTerminal(new_nonterm.to_owned(), annotation.clone()),
                            ],
                            Annotation { name: None },
                        ),
                    });
                    new_exprs.push(Expression {
                        lhs: new_nonterm.to_owned(),
                        rhs: EBNFTokenNode::Epsilon,
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
                        rhs: EBNFTokenNode::NonTerminal(new_nonterm.to_owned(), annotation.clone()),
                    });
                    new_exprs.push(Expression {
                        lhs: new_nonterm.to_owned(),
                        rhs: EBNFTokenNode::Multiple(
                            vec![
                                (*b).clone(),
                                EBNFTokenNode::NonTerminal(new_nonterm.to_owned(), annotation.clone()),
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
        EBNFTokenNode::Alternation(a, b, c) => {
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
        EBNFTokenNode::Group(a, _) => {
            new_exprs.push(Expression {
                lhs: rule.lhs.to_owned(),
                rhs: *a,
            });
            any = true;
        }
        EBNFTokenNode::Fmt(_) => {
            new_exprs.push(rule);
        }
        EBNFTokenNode::Unknown => todo!(),
    }
    return (new_exprs, any);
}

#[cfg(test)]
mod test {
    use crate::get_grammar;

    use super::*;
    use insta::assert_yaml_snapshot;
    /*
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
    #[test]
    fn multiple_plus() {
        let source = r"
         A ::= (B C)+;
     ";
        let result = to_nf_grammar(get_grammar(source).unwrap());
        assert_yaml_snapshot!(result)
    }
    */
}
