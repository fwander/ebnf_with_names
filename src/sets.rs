fn compute_first_set(node: &NFNode, expressions: &[Expression]) -> HashSet<Token> {
    let mut first_set = HashSet::new();
    match node {
        NFNode::Terminal(token) => {
            first_set.insert(token.clone());
        }
        NFNode::NonTerminal(symbol) => {
            let exprs_with_symbol = expressions.iter().filter(|e| e.lhs == *symbol);
            for expr in exprs_with_symbol {
                let rhs_first_set = compute_first_set(&expr.rhs, expressions);
                first_set.extend(rhs_first_set);
            }
        }
        NFNode::Multiple(node1, node2) => {
            let node1_first_set = compute_first_set(node1, expressions);
            first_set.extend(node1_first_set);
            if node1_first_set.contains(&Token::Epsilon) {
                let node2_first_set = compute_first_set(node2, expressions);
                first_set.extend(node2_first_set);
            }
        }
        NFNode::Epsilon => {
            first_set.insert(Token::Epsilon);
        }
    }
    first_set
}