use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::{
        complete::{self, alphanumeric0},
        streaming::{alphanumeric1, char},
    },
    combinator::{all_consuming, opt, recognize},
    error::{VerboseError, VerboseErrorKind},
    multi::many1,
    sequence::{delimited, preceded, terminated},
    Err, IResult,
};

use parse_hyperlinks::take_until_unbalanced;

use crate::{
    node::{Annotation, EBNFNode, RegexExtKind},
    Expression,
};

type Res<T, U> = IResult<T, U, VerboseError<T>>;

fn parse_lhs(input: &str) -> Res<&str, String> {
    let (input, lhs) = preceded(
        complete::multispace0,
        many1(alt((complete::alphanumeric1, tag("_")))),
    )(input)?;
    let (input, _) = preceded(complete::multispace0, alt((tag("="), tag("::="))))(input)?;

    Ok((input, lhs.join("").trim_end().to_owned()))
}

fn parse_rhs(input: &str) -> Res<&str, EBNFNode> {
    let (input, rhs) = preceded(
        complete::multispace0,
        terminated(
            parse_multiple,
            preceded(complete::multispace0, complete::char(';')),
        ),
    )(input)?;

    Ok((input, rhs))
}

fn parse_string(input: &str) -> Res<&str, EBNFNode> {
    let (input, string) = alt((
        delimited(complete::char('\''), take_until("'"), complete::char('\'')),
        delimited(complete::char('"'), take_until("\""), complete::char('"')),
    ))(input)?;
    let (input, tag) = parse_tag(input)?;

    Ok((
        input,
        EBNFNode::Terminal(
            string.to_string(),
            Annotation {
                name: Some(tag.to_string()),
            },
        ),
    ))
}

fn parse_regex_string(input: &str) -> Res<&str, EBNFNode> {
    let (input, string) = alt((
        delimited(tag("#'"), take_until("'"), complete::char('\'')),
        delimited(tag("#\""), take_until("\""), complete::char('"')),
    ))(input)?;
    let (input, tag) = parse_tag(input)?;

    Ok((
        input,
        EBNFNode::RegexString(
            string.to_string(),
            Annotation {
                name: Some(tag.to_string()),
            },
        ),
    ))
}

fn parse_new_line(input: &str) -> Res<&str, EBNFNode> {
    let (input, symbol) = preceded(
        complete::multispace0,
        preceded(tag("\\"), complete::alphanumeric1),
    )(input)?;

    Ok((input, EBNFNode::Fmt(symbol.to_string())))
}

fn parse_terminal(input: &str) -> Res<&str, EBNFNode> {
    let (input, symbol) = preceded(
        complete::multispace0,
        terminated(
            many1(alt((complete::alphanumeric1, tag("_")))),
            complete::multispace0,
        ),
    )(input)?;
    let (input, tag) = parse_tag(input)?;

    Ok((
        input,
        EBNFNode::NonTerminal(
            symbol.join(""),
            Annotation {
                name: Some(tag.to_string()),
            },
        ),
    ))
}

fn parse_multiple(input: &str) -> Res<&str, EBNFNode> {
    let (input, node) = preceded(complete::multispace0, many1(parse_node))(input)?;

    match node {
        _ if node.len() == 1 => Ok((input, node[0].clone())),
        _ => Ok((
            input,
            EBNFNode::Multiple(
                node,
                Annotation {
                    name: Some("".to_string()),
                },
            ),
        )),
    }
}

fn parse_node(input: &str) -> Res<&str, EBNFNode> {
    let (mut input, mut left_node) = preceded(
        complete::multispace0,
        alt((
            parse_group,
            parse_string,
            parse_regex_string,
            parse_terminal,
            parse_new_line,
        )),
    )(input)?;

    let optional_regex_ext: Res<&str, RegexExtKind> = parse_regex_ext(input);

    match optional_regex_ext {
        Ok(((s, regex_ext_kind))) => {
            input = s;
            let (input_new, tag) = parse_tag(input)?;
            input = input_new;
            left_node = EBNFNode::RegexExt(
                Box::new(left_node),
                regex_ext_kind,
                Annotation {
                    name: Some(tag.to_string()),
                },
            );
        }
        Err(_) => {}
    }

    let optional_symbol: Res<&str, EBNFNode> = parse_symbol(input);

    match optional_symbol {
        Ok((input, right_node)) => Ok((
            input,
            EBNFNode::Alternation(
                Box::new(left_node),
                Box::new(right_node),
                Annotation {
                    name: Some("".to_string()),
                },
            ),
        )),
        Err(_) => Ok((input, left_node)),
    }
}

fn parse_regex_ext(input: &str) -> Res<&str, RegexExtKind> {
    let (input, regex_ext) = preceded(
        complete::multispace0,
        alt((
            complete::char('*'),
            complete::char('+'),
            complete::char('?'),
        )),
    )(input)?;

    let regex_kind = match regex_ext {
        '*' => RegexExtKind::Repeat0,
        '+' => RegexExtKind::Repeat1,
        '?' => RegexExtKind::Optional,
        _ => unreachable!("Unexpected regex extension symbol. this should not happen"),
    };

    Ok((input, regex_kind))
}

fn parse_symbol(input: &str) -> Res<&str, EBNFNode> {
    let (input, symbol_pair) = preceded(complete::multispace0, parse_alternation)(input)?;

    Ok((input, symbol_pair))
}

fn parse_alternation(input: &str) -> Res<&str, EBNFNode> {
    let (input, node) = preceded(complete::char('|'), parse_node)(input)?;

    Ok((input, node))
}

fn parse_delimited_node(
    input: &str,
    opening_bracket: char,
    closing_bracket: char,
) -> Res<&str, &str> {
    let result = delimited(
        tag(opening_bracket.to_string().as_str()),
        take_until_unbalanced(opening_bracket, closing_bracket),
        tag(closing_bracket.to_string().as_str()),
    )(input);

    match result {
        Ok((input, inner)) => Ok((input, inner)),
        Err(_) => Err(Err::Error(VerboseError {
            errors: vec![(
                input,
                VerboseErrorKind::Context("Incomplete delimited node"),
            )],
        })),
    }
}

fn parse_tag(input: &str) -> Res<&str, &str> {
    let (input, name) = opt(preceded(
        complete::multispace0,
        delimited(tag("<"), alphanumeric0, tag(">")),
    ))(input)?;
    Ok((input, name.unwrap_or("")))
}

fn parse_group(input: &str) -> Res<&str, EBNFNode> {
    let (input, inner) = parse_delimited_node(input, '(', ')')?;
    let (_, node) = preceded(complete::multispace0, parse_multiple)(inner)?;
    let (input, tag) = parse_tag(input)?;

    Ok((
        input,
        EBNFNode::Group(
            Box::new(node),
            Annotation {
                name: Some(tag.to_string()),
            },
        ),
    ))
}

pub(crate) fn parse_expressions(input: &str) -> Res<&str, Vec<Expression<EBNFNode>>> {
    let mut source = input;
    let mut expressions = Vec::<Expression<EBNFNode>>::new();

    while !source.is_empty() {
        let (input, lhs) = parse_lhs(source)?;
        let (input, rhs) = parse_rhs(input)?;

        expressions.push(Expression {
            lhs: lhs.to_string(),
            rhs,
        });

        source = input.trim_start();
    }

    Ok((input, expressions))
}

#[cfg(test)]
mod test {
    use crate::parser::parse_expressions;

    use insta::assert_yaml_snapshot;

    #[test]
    fn test_parse() {
        let src = r"
            Filter ::= ( First SPACE )? ( Number '~ ' )? ( Number '-' Number ) ( ' ' Number '~' )? ( ' Hz' )? ( ' B' )? ( ' I' )? ( ' A' )?;
            First  ::= #'[A-Za-z][A-Za-z0-9_+]*';
            Number ::= Digits ( K Digits? )?;
            Digits ::= (#'[0-9]+' | 'm') <Digit>;
            SPACE ::= ' ';

            TOK1 -> #'[0-9]+'
            TOK2 -> 'm'

            K ::= J | L

            abcd[ID] abcdf[ID] if[KEYWORD, ID]

            K => #'\.|,'
            J ::= ','
            L ::= '.'
            .[L, K] ,[J, K] .[L, K]
        ";

        let (input, vec) = parse_expressions(src).unwrap();

        println!("{:#?}", vec);
    }
    #[test]
    fn simple_alternation() {
        let source = r"
         filter ::= a | b;
         a ::= 'a';
         b ::= 'b';
     ";
        let result = parse_expressions(source).unwrap();
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn space_before_semi() {
        let source = r"
         filter ::= a | b ;
         a ::= 'a';
         b ::= 'b';
     ";
        let result = parse_expressions(source).unwrap();
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn underscore() {
        let source = r"
         filter ::= a_b;
         a_b ::= 'a' | 'b';
     ";
        let result = parse_expressions(source).unwrap();
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn multiple_underscores() {
        let source = r"
         filter ::= a_b_cat;
         a_b_cat ::= 'a' | 'b';
     ";
        let result = parse_expressions(source).unwrap();
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn annotate() {
        let source = r"
         filter ::= a_b_cat <name>;
         a_b_cat ::= 'a' <0> | 'b' <1>;
     ";
        let result = parse_expressions(source).unwrap();
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn annotate_list() {
        let source = r"
         filter ::= a_b_cat+ <name>;
         a_b_cat ::= 'a' <0> | 'b' <1>;
     ";
        let result = parse_expressions(source).unwrap();
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn annotate_list_nested() {
        let source = r"
         filter ::= (a_b_cat 'tag' <nameInner> )+ <name>;
         a_b_cat ::= 'a' <0> | 'b' <1>;
     ";
        let result = parse_expressions(source).unwrap();
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn annotate_list_nested_group() {
        let source = r"
         filter ::= (a_b_cat ('tag') <nameInner> )+ <name>;
         a_b_cat ::= 'a' <0> | 'b' <1>;
     ";
        let result = parse_expressions(source).unwrap();
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn fmt_n() {
        let source = r"
         filter ::= 'a:' \n 'b:';
     ";
        let result = parse_expressions(source).unwrap();
        assert_yaml_snapshot!(result)
    }
    #[test]
    fn alternation_precidence() {
        let source = r"
         filter ::= 'a' | 'b' 'c';
     ";
        let result = parse_expressions(source).unwrap();
        assert_yaml_snapshot!(result)
    }
}
