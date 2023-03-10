use std::{collections::HashMap, fs, path::PathBuf};

use once_cell::sync::Lazy;
use tera::{Context, Tera};

use crate::config;

pub static TERA: Lazy<Tera> = Lazy::new(|| {
    let mut tera = match Tera::new("*(N)") {
        Ok(t) => t,
        Err(e) => {
            println!("Parsing error(s): {}", e);
            ::std::process::exit(1);
        }
    };

    tera.add_raw_template(
        "component",
        r"
export const {{name}}: React.FC<ComponentInput> = (props) => {
  let expr = props.current as AST_node;
  {%- for select in selects%}
  const [select{{select[0]}}, setSelect{{select[0]}}] = useState(-1);
  expr.selection_state[{{select[1]}}] = select{{select[0]}};
  {%- endfor %}
  {%- for focus in focusi%}
  const [focus{{select[0]}}, setSelect{{select[0]}}] = useState(-1);
  expr.selection_state[{{select[1]}}] = select{{select[0]}};
  {%- endfor %}
  props.setCurrent(expr);
  useEffect(()=>{return ()=>{update_root();update_visual();}},[]);
  return {{jsx}};
}
",
    )
    .unwrap();

    tera.add_raw_template(
        "AST",
        r"
export class {{name}}_node extends AST_node {
  constructor(parent: AST_node|undefined){
      super(parent);
  }
  accept = (v : Visitor<any>) => { return v.visit{{name}}_node(this); }
  id = '{{name}}';
  child: {{type}}|[] = [];
  {%- for tag in tags %}
  {{tag}}
  {%- endfor %}
  clone(parent?: AST_node){
    let ret = new {{name}}_node(parent);
    let temp: any;
    ret.selection_state = this.selection_state;
    {{clone}}
    return ret;
  }
}
",
    )
    .unwrap();

    tera.add_raw_template(
        "visitor.ts",
        r"
import { AST_node } from './ast';
import { 
  {%- for name in names -%}
  {{name}}_node,
  {%- endfor -%}
  } from './ast';
export abstract class Visitor<ResultType> {
  protected abstract noVal: ResultType;
  try(node: any, def?: ResultType): ResultType{
    if (node instanceof AST_node){
      return node.accept(this);
    }
    return def? def : this.noVal;
  }
  is_undefined(node: any){
    if (node === undefined || (Array.isArray(node) && node.length === 0)){
      return true;
    }
    return false;
  }
  get(node: any, def?: ResultType): ResultType{
    let ret = node as ResultType;
    if (this.is_undefined(ret)){
      return def? def : this.noVal;
    }
    return ret;
  }
  {%- for name in names %}
  abstract visit{{name}}_node (node : {{name}}_node) : ResultType;
  {%- endfor %}
}

export class DefaultChecker extends Visitor<boolean>{
    protected noVal: boolean = true;
    {%- for name in names%}
    visit{{name}}_node(node: {{name}}_node): boolean {
        return true;
    }
    {%- endfor %}
}
",
    )
    .unwrap();

    tera.add_raw_template(
        "ast.css",
        r"
{% for name in names%}
.{{name}} {
  background: rgb(102, 102, 153, .2);
  display: table-cell
}
{% endfor %}
    ",
    )
    .unwrap();

    tera.add_raw_template(
        "components.tsx",
        std::str::from_utf8(include_bytes!("templates/components.tsx")).unwrap_or(""),
    )
    .unwrap();
    tera.add_raw_template(
        "ast.ts",
        std::str::from_utf8(include_bytes!("templates/ast.ts")).unwrap_or(""),
    )
    .unwrap();
    //tera.add_raw_template("visitor",std::str::from_utf8(include_bytes!("templates/visitor")).unwrap_or("")).unwrap();
    //tera.add_raw_template("astcss",std::str::from_utf8(include_bytes!("templates/astcss")).unwrap_or("")).unwrap();
    tera.add_raw_template(
        "focus.ts",
        std::str::from_utf8(include_bytes!("templates/focus.ts")).unwrap_or(""),
    )
    .unwrap();
    tera.add_raw_template(
        "lib.css",
        std::str::from_utf8(include_bytes!("templates/lib.css")).unwrap_or(""),
    )
    .unwrap();
    tera.add_raw_template(
        "select.css",
        std::str::from_utf8(include_bytes!("templates/select.css")).unwrap_or(""),
    )
    .unwrap();
    tera.add_raw_template(
        "Select.tsx",
        std::str::from_utf8(include_bytes!("templates/Select.tsx")).unwrap_or(""),
    )
    .unwrap();
    tera.add_raw_template(
        "VariableList.tsx",
        std::str::from_utf8(include_bytes!("templates/VariableList.tsx")).unwrap_or(""),
    )
    .unwrap();
    tera.add_raw_template(
        "Terminal.tsx",
        std::str::from_utf8(include_bytes!("templates/Terminal.tsx")).unwrap_or(""),
    )
    .unwrap();
    tera.add_raw_template(
        "Regex.tsx",
        std::str::from_utf8(include_bytes!("templates/Regex.tsx")).unwrap_or(""),
    )
    .unwrap();
    tera.add_raw_template(
        "lib.tsx",
        std::str::from_utf8(include_bytes!("templates/lib.tsx")).unwrap_or(""),
    )
    .unwrap();

    tera
});

fn get_impl(name: &str, impls: &Vec<config::Impl>, default: String) -> Option<String> {
    for i in impls {
        for terminal in &i.applies_to {
            if terminal == "*" || *terminal == *name {
                return Some(
                    i.loc
                        .file_name()?
                        .to_str()?
                        .to_string()
                        .strip_suffix(".tsx")?
                        .to_string(),
                );
            }
        }
    }
    return Some(default);
}

fn add_imports(imports: &mut Vec<String>, impls: &Vec<config::Impl>, dest: &PathBuf) {
    for i in impls {
        let path = get_relative_path(dest, &i.loc).unwrap();
        let choice_path = path
            .to_str()
            .unwrap()
            .to_string()
            .strip_suffix(".tsx")
            .unwrap()
            .to_string();
        let choice_name = path
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string()
            .strip_suffix(".tsx")
            .unwrap()
            .to_string();
        imports.push(format!("import {choice_name} from \"{choice_path}\""));
    }
}

fn upper(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn get_relative_path(from_path: &PathBuf, to_path: &PathBuf) -> Option<PathBuf> {
    let mut from_iter = from_path.iter().peekable();
    let mut to_iter = to_path.iter().peekable();

    while let (Some(from_component), Some(to_component)) = (from_iter.peek(), to_iter.peek()) {
        if from_component != to_component {
            break;
        } else {
            from_iter.next();
            to_iter.next();
        }
    }

    let mut remaining = from_iter.count();

    let mut relative_path = PathBuf::new();
    while remaining > 0 {
        relative_path.push("..");
        remaining -= 1;
    }

    for component in to_iter {
        relative_path.push(component);
    }

    if relative_path.components().count() > 0 {
        Some(relative_path)
    } else {
        None
    }
}

fn alternation_to_list(node: &ebnf::Node) -> Result<Vec<&Box<ebnf::Node>>, i32> {
    match node {
        ebnf::Node::Symbol(l, _, r) => {
            let mut ret = vec![l];
            let mut looking_at = r;
            loop {
                match &**looking_at {
                    ebnf::Node::Symbol(l, _, r) => {
                        ret.push(&l);
                        looking_at = &r;
                    }
                    _ => {
                        ret.push(looking_at);
                        break;
                    }
                }
            }
            return Ok(ret);
        }
        _ => return Err(0),
    }
}

#[derive(Clone)]
enum GetterStep {
    Check(i32),
    Index(i32),
    Variable,
    Final(String),
}

//for every tagged node (node with a proceeding <name>) return a way to access that node through a
//series of check(num) index(num) and variable inputs then finaly the type of that node
fn node_to_getter_info<'a>(node: &'a ebnf::Node) -> HashMap<&'a String, Vec<GetterStep>> {
    fn node_to_getter_helper<'a>(
        node: &'a ebnf::Node,
        ret: &mut HashMap<&'a String, Vec<GetterStep>>,
        acc: &Vec<GetterStep>,
    ) {
        let mut tmp_acc = acc.to_owned();
        tmp_acc.push(GetterStep::Final(type_of(node)));
        match node {
            ebnf::Node::String(_s, tag) | ebnf::Node::Terminal(_s, tag) => {
                if !tag.is_empty() {
                    ret.insert(tag, tmp_acc);
                    ()
                } else {
                    ()
                }
            }
            ebnf::Node::RegexString(_s, tag) => {
                if !tag.is_empty() {
                    ret.insert(tag, tmp_acc);
                    ()
                } else {
                    ()
                }
            }
            ebnf::Node::Terminal(_s, tag) => {
                if !tag.is_empty() {
                    ret.insert(tag, tmp_acc);
                    ()
                } else {
                    ()
                }
            }
            ebnf::Node::Multiple(nodes) => {
                let mut index = 0;
                let _ = nodes
                    .iter()
                    .enumerate()
                    .map(|(i, n)| {
                        match n {
                            ebnf::Node::Fmt(_) => (),
                            _ => index += 1,
                        };
                        node_to_getter_helper(
                            n,
                            ret,
                            &[&acc[..], &[GetterStep::Index(index - 1 as i32)]].concat(),
                        )
                    })
                    .for_each(drop);
                ()
            }
            ebnf::Node::RegexExt(n, kind, tag) => {
                match kind {
                    ebnf::RegexExtKind::Optional => {
                        node_to_getter_helper(
                            n,
                            ret,
                            &[&acc[..], &[GetterStep::Check(0)]].concat(),
                        );
                    }
                    _ => {
                        node_to_getter_helper(
                            n,
                            ret,
                            &[&acc[..], &[GetterStep::Variable]].concat(),
                        );
                    }
                }
                if !tag.is_empty() {
                    ret.insert(tag, tmp_acc);
                    ()
                } else {
                    ()
                }
            }
            ebnf::Node::Symbol(_left, _kind, _right) => {
                let v = alternation_to_list(node).unwrap();
                v.iter()
                    .enumerate()
                    .map(|(i, n)| {
                        node_to_getter_helper(
                            n,
                            ret,
                            &[&acc[..], &[GetterStep::Check(i as i32)]].concat(),
                        )
                    })
                    .for_each(drop);
                ()
            }
            ebnf::Node::Group(n, tag) => {
                node_to_getter_helper(n, ret, acc);
                if !tag.is_empty() {
                    ret.insert(tag, tmp_acc);
                    ()
                } else {
                    ()
                }
            }
            ebnf::Node::Optional(_n, tag) => {
                if !tag.is_empty() {
                    ret.insert(tag, tmp_acc);
                    ()
                } else {
                    ()
                }
            }
            ebnf::Node::Repeat(n, _) => {
                node_to_getter_helper(n, ret, acc);
                ()
            }
            ebnf::Node::Fmt(_) => (),
            ebnf::Node::Unknown => (),
        };
    }
    let mut ret = HashMap::new();
    node_to_getter_helper(node, &mut ret, &vec![]);
    ret
}

// want to generate <name>(<params>){if this.check(<checking>) then return this.get(<args>) else return undefined}
fn getter_info_to_jsx(getter_info: &HashMap<&String, Vec<GetterStep>>) -> Vec<String> {
    let mut ret = vec![];
    for (name, steps) in getter_info {
        let mut params: String = "".to_string();
        let mut checking: String = "".to_string();
        let mut args: String = "".to_string();
        let mut vars_seen = 0;
        let mut node_type: String = "".to_string();
        for step in steps {
            match step {
                GetterStep::Check(n) => checking += &format!("{},", n),
                GetterStep::Variable => {
                    params += &format!("x{}: number,", vars_seen);
                    args += &format!("x{},", vars_seen);
                    vars_seen += 1
                }
                GetterStep::Index(n) => args += &format!("{},", n),
                GetterStep::Final(t) => node_type = t.to_string(),
            }
        }
        ret.push(format!(
            "{name}({params}): {node_type}|undefined{{if (this.check([{args}],[{checking}])) return this.get([{args}]); else return undefined;}}"));
        ret.push(format!(
            "set_{name}({params}val: {node_type}){{if (this.check([{args}],[{checking}])) {{this.set(val,[{args}]); return true;}} else return false;}}"));
    }
    ret
}

fn list_to_str(input: &Vec<&str>) -> String {
    format!(
        "[{}]",
        input
            .iter()
            .fold(String::new(), |acc, val| acc + val + ", ")
    )
}

fn node_to_jsx(node: &ebnf::Node, name: &str, description: &config::NodeDescription) -> String {
    fn focus_format(s: &str, push: bool) -> String {
        return format!("<Focusable {{...new FocusableInput({push})}}>\n<>{s}</>\n</Focusable>\n");
    }

    //returns a string representing the body of the jsx
    fn node_to_jsx_helper(
        node: &ebnf::Node,
        mult_index: &Vec<&str>,
        last_variatic: i32,
        first: bool,
        description: &config::NodeDescription,
        select_depth: i32,
    ) -> String {
        return match node {
            ebnf::Node::String(s, _) => {
                format!(
                    "<Default {{...new DefaultInput(expr,{},'{}',{})}}/>",
                    list_to_str(mult_index),
                    s,
                    description.terminal,
                )
            }
            ebnf::Node::RegexString(s, _) => {
                format!(
                    "<TextInput {{...new TextInputInput(expr,{},/^{}$/,{})}}/>",
                    list_to_str(mult_index),
                    s,
                    description.regex,
                )
            }
            ebnf::Node::Terminal(s, _) => {
                format!(
                    "<{} {{...new ComponentInput(expr,{},new {}_node(expr))}}/>",
                    upper(s),
                    list_to_str(mult_index),
                    s,
                )
            }
            ebnf::Node::Multiple(nodes) => {
                let mut index = 0;
                "<SelectionContext.Provider value = {null}>".to_string()
                + "<div className=\"multiple\">"
                    + &nodes
                        .iter()
                        .fold("".to_string(), |c: String, n| {
                            match n {
                                ebnf::Node::Fmt(_) => (),
                                _ => index += 1,
                            };
                            c + "\n"
                                + &node_to_jsx_helper(
                                    n,
                                    &[&mult_index[..], &[&(index - 1).to_string()]].concat(),
                                    last_variatic,
                                    false,
                                    description,
                                    select_depth,
                                )
                        })
                    + "\n</div>"
                    + "\n</SelectionContext.Provider>"
            }
            ebnf::Node::RegexExt(n, kind, _) => {
                match kind {
                    ebnf::RegexExtKind::Optional => {
                        focus_format(
                            &format!("<Choice\n{{...\nnew ChoiceInput((n:number)=>{{expr.add_selection([{vars}],{depth},n)}},[{names}],[{outputs}],{format})\n}}\n/>", 
                                format = description.choice,
                                names = format!("\"{}\"",&node_to_string(n)),
                                outputs = &node_to_jsx_helper(
                                            n,
                                            mult_index,
                                            last_variatic,
                                            false,
                                            description,
                                            select_depth+1),
                                vars = &(0..last_variatic).fold("".to_string(), |c,n| c + &format!("n{n},")),
                                depth = select_depth,
                            ) , true)
                    },
                    _ => {
                        let mut min = 0;
                        if matches!(kind, ebnf::RegexExtKind::Repeat1) {
                            min = 1;
                        }
                        let mut adding = mult_index.clone();
                        let last = 'n'.to_string() + &last_variatic.to_string();
                        adding.push(&(last));
                        format!("<Variatic {{...new VariaticInput((n{x}:number)=>{{\nreturn <div key={{n{x}}}>\n{body}</div>}},{min},{odd},()=>{{expr.add_selection([{args}],{depth})}},{format})}}/>\n" ,
                            x = last_variatic,
                            body = &node_to_jsx_helper(n, &adding, last_variatic+1,false, description, 0),
                            args = &(0..last_variatic).fold("".to_string(), |c,n| c + &format!("n{n},")), 
                            depth = select_depth,
                            format = description.list,
                            odd = (last_variatic%2)==0,
                            )
                    }
                }
            }
            ebnf::Node::Symbol(_left, _kind, _right) => {
                let v = alternation_to_list(node).unwrap();
                        format!("<Choice\n{{...\nnew ChoiceInput((n:number)=>{{expr.add_selection([{vars}],{depth},n)}},[{names}],[{outputs}],{format})\n}}\n/>", 
                            format = description.choice,
                            names =  v.iter().fold("".to_string(), |c, n| c + "\"" + &node_to_string(n) + "\", "),
                            outputs = v.iter().fold("".to_string(), |c, n| 
                                c  + &node_to_jsx_helper(
                                        n,
                                        mult_index,
                                        last_variatic,
                                        false,
                                        description,
                                        select_depth+1,
                                    )
                                    + ",\n"),
                            vars = &(0..last_variatic).fold("".to_string(), |c,n| c + &format!("n{n},")),
                            depth = select_depth,
                        )
            }
            ebnf::Node::Group(n, _) => node_to_jsx_helper(
                n,
                mult_index,
                last_variatic,
                first,
                description,
                select_depth,
            ),
            ebnf::Node::Optional(n, _) => {
                let name = match &**n {
                    ebnf::Node::Terminal(s, _) => s.to_string(),
                    _ => "".to_string(),
                };
                    format!("<Before {{...new BeforeInput(expr,{mult_index},'{name}', {choice_format}, {default_format})}}/>",
                    mult_index = list_to_str(mult_index),
                    name = name,
                    choice_format = description.choice,
                    default_format = description.terminal,
                )
            }
            ebnf::Node::Fmt(symbol) => match symbol.as_str() {
                "n" => "</div>\n<div className=\"multiple\">".to_string(),
                _ => "".to_string(),
            },
            _ => "".to_string(),
        };
    }
    let body = &node_to_jsx_helper(node, &vec![], 0, true, description, 0);
    return format!(
        "
  export const {component_name}: React.FC<ComponentInput> = (props) => {{
    let expr = props.current as AST_node;
    props.setCurrent(expr);
    useEffect(()=>{{return ()=>{{update_root();update_visual();}}}},[]);
    return <div className=\"{name}\">\n {body} \n</div>
  }}",
        name = name,
        component_name = &upper(&name),
        body = body,
    );
}

fn node_to_clone(node: &ebnf::Node) -> String {
    fn node_to_clone_helper(
        node: &ebnf::Node,
        select_depth: i32,
        index: String,
        loopCounter: i32,
    ) -> String {
        return match node {
            ebnf::Node::String(s, _) => format!("ret.set(this.get([{index}]),[{index}]);",index=index),
            ebnf::Node::RegexString(s, _) => format!("ret.set(this.get([{index}]),[{index}]);",index=index),
            ebnf::Node::Terminal(s, _) => format!("temp = this.get([{index}]);\nif (temp instanceof {name}_node) ret.set(temp.clone(this),[{index}]);",index=index,name=s),
            ebnf::Node::Multiple(nodes) => {
                let mut acc = -1;
                nodes
                .iter()
                .fold("".to_string(), |c: String, n| 
                    {match n {
                        ebnf::Node::Fmt(_) => (),
                        _ => acc += 1,
                    };
                    c + &node_to_clone_helper(n,select_depth,index.to_owned() + acc.to_string().as_str() + ", ",loopCounter) + "\n"})},
            ebnf::Node::RegexExt(n, _kind, _) => {
                format!("for (let x{n} = 0; x{n} < this.get([{index}]).length; x{n}++) {{\n {body} }}\n",n=loopCounter,index=index,body=node_to_clone_helper(n, select_depth, index.to_owned() + "x" + loopCounter.to_string().as_str() + ", ", loopCounter+1))
            },
            ebnf::Node::Symbol(_left, _kind, _right) => {
                let v = alternation_to_list(node).unwrap();
                format!("switch(this.get_selection([{}],{})) {{\n {} }}\n",
                    (0..loopCounter).fold("".to_string(), |c, i| c + &format!("x{},",i)),select_depth,v.into_iter().enumerate().fold(
                        "".to_string(),
                        |c,(i,n)|
                        format!("{pre} case {n}: {body} break;\n",pre=c,n=i,body=&node_to_clone_helper(n, select_depth+1, index.to_owned(), loopCounter))
                    ))
            }
            ebnf::Node::Group(n, _) => node_to_clone_helper(n, select_depth, index, loopCounter),
            ebnf::Node::Optional(n, _) => node_to_clone_helper(n, select_depth, index, loopCounter),
            _ => "".to_string(),
        };
    }
    return node_to_clone_helper(node, 0, "".to_string(), 0);
}

fn node_to_string(node: &ebnf::Node) -> String {
    return match node {
        ebnf::Node::String(s, _) => s.to_owned(),
        ebnf::Node::RegexString(s, _) => s.to_owned(),
        ebnf::Node::Terminal(s, _) => "<".to_string() + s + ">",
        ebnf::Node::Multiple(nodes) => nodes
            .iter()
            .fold("".to_string(), |c: String, n| c + " " + &node_to_string(n)),
        ebnf::Node::RegexExt(n, kind, _) => match kind {
            ebnf::RegexExtKind::Optional => node_to_string(n) + "?",

            _ => node_to_string(n) + "[]",
        },
        ebnf::Node::Symbol(left, _kind, right) => {
            node_to_string(left) + "|" + &node_to_string(right)
        }
        ebnf::Node::Group(n, _) => "(".to_string() + &node_to_string(n) + ")",
        ebnf::Node::Optional(n, _) => "[".to_string() + &node_to_string(n) + "]",
        ebnf::Node::Repeat(n, _) => "{".to_string() + &node_to_string(n) + "}",
        ebnf::Node::Fmt(_) => "".to_string(),
        ebnf::Node::Unknown => "".to_string(),
    };
}

fn type_of(node: &ebnf::Node) -> String {
    fn node_to_type(node: &ebnf::Node) -> String {
        return match node {
            ebnf::Node::String(_s, _) => "string".to_string(),
            ebnf::Node::RegexString(_s, _) => "string".to_string(),
            ebnf::Node::Terminal(s, _) => s.clone() + "_node",
            ebnf::Node::Multiple(nodes) => {
                "[".to_string()
                    + &node_to_type(&nodes[0])
                    + &nodes[1..].iter().fold("".to_string(), |c: String, n| {
                        let res = node_to_type(n);
                        c + match res.as_str() {
                            "" => "",
                            _ => ",",
                        } + &res
                    })
                    + "]"
            }
            ebnf::Node::RegexExt(n, kind, _) => match kind {
                ebnf::RegexExtKind::Optional => node_to_type(n),

                _ => node_to_type(n) + "[]",
            },
            ebnf::Node::Symbol(left, _kind, right) => {
                node_to_type(left) + "|" + &node_to_type(right)
            }
            ebnf::Node::Group(n, _) => "(".to_string() + &node_to_type(n) + ")",
            ebnf::Node::Optional(_n, _) => node_to_type(_n),
            ebnf::Node::Repeat(n, _) => "AST_node".to_string(),
            ebnf::Node::Fmt(_) => "".to_string(),
            ebnf::Node::Unknown => "".to_string(),
        };
    }
    return node_to_type(node);
}

fn expression_to_ast(expression: &ebnf::Expression) -> String {
    let mut context = Context::new();
    context.insert("name", &expression.lhs);
    context.insert("type", &type_of(&expression.rhs));
    let getter_info = node_to_getter_info(&expression.rhs);
    context.insert("tags", &getter_info_to_jsx(&getter_info));
    context.insert("clone", &node_to_clone(&expression.rhs));

    return TERA.render("AST", &context).unwrap();
}

fn expression_to_component(
    expression: &ebnf::Expression,
    description: &config::NodeDescription,
) -> String {
    //context.insert("name", &upper(&expression.lhs));
    //context.insert("astname", &expression.lhs);
    return node_to_jsx(&expression.rhs, &expression.lhs, description);
}

#[cfg(target_os = "windows")]
const PATH_SEPARATOR: &str = "\\";

#[cfg(not(target_os = "windows"))]
const PATH_SEPARATOR: &str = "/";

pub fn render_all(config: config::Config) {
    let output_folder = config.dest.to_str().unwrap();
    let render_file = |name: &str, context: &Context| {
        fs::write(
            &(output_folder.to_owned() + PATH_SEPARATOR + name),
            TERA.render(&("".to_string() + name), context).unwrap(),
        )
        .expect("can't write to file");
    };

    let source = fs::read_to_string(&config.grammar).unwrap();
    let grammar = ebnf::get_grammar(&source).unwrap();

    let mut ast_nodes: Vec<String> = vec![];
    let mut components: Vec<String> = vec![];
    let mut names: Vec<String> = vec![];
    let mut imports: Vec<String> = vec![];
    add_imports(&mut imports, &config.choice, &config.dest);
    add_imports(&mut imports, &config.list, &config.dest);
    add_imports(&mut imports, &config.terminal, &config.dest);
    add_imports(&mut imports, &config.regex, &config.dest);
    grammar.expressions.iter().for_each(|item| {
        let choice_name = get_impl(&item.lhs, &config.choice, "Select".to_string());
        let list_name = get_impl(&item.lhs, &config.list, "VariableList".to_string());
        let terminal_name = get_impl(&item.lhs, &config.terminal, "Terminal".to_string());
        let regex_name = get_impl(&item.lhs, &config.regex, "Regex".to_string());
        let description: config::NodeDescription = config::NodeDescription {
            choice: choice_name.unwrap(),
            list: list_name.unwrap(),
            terminal: terminal_name.unwrap(),
            regex: regex_name.unwrap(),
        };
        components.push(expression_to_component(&item, &description));
        ast_nodes.push(expression_to_ast(&item));
        names.push(item.lhs.to_string());
    });
    match fs::remove_dir_all(&output_folder) {
        Err(_) => (), //ignore error because it's ok if the output folder doesn't exist before we create it :)
        _ => (),
    };
    fs::create_dir(&output_folder).unwrap();

    let mut component_context = Context::new();
    component_context.insert("names", &names);
    component_context.insert("components", &components);
    component_context.insert("imports", &imports);
    let mut css = Context::new();
    css.insert(
        "names",
        &(names
            .iter()
            .map(|s| s.to_lowercase() as String)
            .collect::<Vec<String>>()),
    );
    let mut ast_context = Context::new();
    ast_context.insert("astNodes", &ast_nodes);

    let mut visitor_context = Context::new();
    visitor_context.insert("names", &names);

    let mut lib = Context::new();
    lib.insert("start_comp", &upper(&config.start));
    lib.insert("start", &config.start);

    render_file("components.tsx", &component_context);
    render_file("ast.ts", &ast_context);
    render_file("visitor.ts", &visitor_context);
    render_file("ast.css", &css);
    render_file("focus.ts", &component_context);
    render_file("lib.css", &component_context);
    render_file("select.css", &component_context);
    render_file("Select.tsx", &component_context);
    render_file("VariableList.tsx", &component_context);
    render_file("Terminal.tsx", &component_context);
    render_file("Regex.tsx", &component_context);
    render_file("lib.tsx", &lib);
}
