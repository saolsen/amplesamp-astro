// todo: Write a better parser!
// Writing this took me way more time than just using recursive descent would have. Mistake!
use std::fmt::Write;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{char, digit1, line_ending, one_of, space1},
    combinator::{eof, map, opt, recognize},
    error::{context, ContextError, ParseError, VerboseError, VerboseErrorKind},
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    Finish, IResult, InputTake, Offset,
};
use nom_locate::LocatedSpan;

use crate::ast::*;
use crate::error::Error;

type Span<'a> = LocatedSpan<&'a str>;

fn source_info<'a>(input: &Span<'a>) -> SourceInfo<'a> {
    SourceInfo {
        fragment: input.fragment(),
        line: input.location_line(),
        col: input.get_utf8_column() as u32,
    }
}

fn ast<'a, T: PartialEq>(input: Span<'a>, node: T) -> Src<'a, T> {
    Src {
        src: source_info(&input),
        node,
    }
}

fn ast_from_to<'a, T: PartialEq>(input: Span<'a>, end: Span<'a>, node: T) -> Src<'a, T> {
    let offset = end.location_offset() - input.location_offset();
    let inner = input.take(offset);
    Src {
        src: source_info(&inner),
        node,
    }
}

fn wrap_ast<'a, T: PartialEq>(parent_si: SourceInfo<'a>, node: T) -> Src<'a, T> {
    Src {
        src: parent_si,
        node,
    }
}

pub fn convert_error<'a>(input: Span<'a>, e: VerboseError<Span<'a>>) -> Vec<Error> {
    let mut errors = vec![];

    let mut result = String::new();

    for (i, (substring, kind)) in e.errors.iter().enumerate() {
        let offset = input.offset(substring);

        if input.is_empty() {
            match kind {
                VerboseErrorKind::Char(c) => {
                    write!(&mut result, "{}: expected '{}', got empty input\n\n", i, c)
                }
                VerboseErrorKind::Context(s) => {
                    write!(&mut result, "{}: in {}, got empty input\n\n", i, s)
                }
                VerboseErrorKind::Nom(e) => {
                    write!(&mut result, "{}: in {:?}, got empty input\n\n", i, e)
                }
            }
            .unwrap();
        } else {
            let prefix = &input.as_bytes()[..offset];

            // Count the number of newlines in the first `offset` bytes of input
            let line_number = prefix.iter().filter(|&&b| b == b'\n').count() + 1;

            // Find the line that includes the sub-slice:
            // Find the *last* newline before the substring starts
            let line_begin = prefix
                .iter()
                .rev()
                .position(|&b| b == b'\n')
                .map(|pos| offset - pos)
                .unwrap_or(0);

            // Find the full line after that newline
            let line = input[line_begin..]
                .lines()
                .next()
                .unwrap_or(&input[line_begin..])
                .trim_end();

            // The (1-indexed) column number is the offset of our substring into that line
            let column_number = line.offset(substring) + 1;

            let mut msg = String::new();

            let care = match kind {
                VerboseErrorKind::Char(c) => {
                    if let Some(actual) = substring.chars().next() {
                        write!(
                            &mut msg,
                            "at line {line_number}:\n\
                 {line}\n\
                 {caret:>column$}\n\
                 expected '{expected}', found {actual}\n\n",
                            line_number = line_number,
                            line = line,
                            caret = '^',
                            column = column_number,
                            expected = c,
                            actual = actual,
                        )
                        .unwrap();
                        true
                    } else {
                        write!(
                            &mut msg,
                            "at line {line_number}:\n\
                 {line}\n\
                 {caret:>column$}\n\
                 expected '{expected}', got end of input\n\n",
                            line_number = line_number,
                            line = line,
                            caret = '^',
                            column = column_number,
                            expected = c,
                        )
                        .unwrap();
                        true
                    }
                }
                VerboseErrorKind::Context(s) => {
                    write!(
                        &mut msg,
                        "at line {line_number}, in {context}:\n\
               {line}\n\
               {caret:>column$}\n\n",
                        line_number = line_number,
                        context = s,
                        line = line,
                        caret = '^',
                        column = column_number,
                    )
                    .unwrap();
                    true
                }
                VerboseErrorKind::Nom(_e) => {
                    write!(
                        &mut msg,
                        "{i}: at line {line_number}, in {nom_err:?}:\n\
                   {line}\n\
                   {caret:>column$}\n\n",
                        i = i,
                        line_number = line_number,
                        nom_err = e,
                        line = line,
                        caret = '^',
                        column = column_number,
                    )
                    .unwrap();
                    true
                }
            };
            if care {
                write!(&mut result, "{}", msg).unwrap();
                errors.push(Error::Parse {
                    message: msg,
                    line: line_number as u32,
                    column: column_number as u32,
                });
            }
        }
    }
    errors
    //result
}

fn comment<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Span<'a>, E> {
    context("comment", recognize(pair(char('#'), is_not("\n"))))(input)
}

fn opt_comment_newline<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Span<'a>, E> {
    recognize(tuple((opt(comment), line_ending)))(input)
}

fn ws<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Span<'a>, E> {
    alt((opt_comment_newline, space1, tag(";")))(input)
}

fn ws1<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Span<'a>, E> {
    recognize(many1(ws))(input)
}

fn ws0<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Span<'a>, E> {
    recognize(many0(ws))(input)
}

fn name<'a>(input: Span<'a>) -> IResult<Span<'a>, Span<'a>, VerboseError<Span<'a>>> {
    recognize(pair(
        one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"),
        many0(one_of(
            "_-abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789",
        )),
    ))(input)
}

fn integer<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Src<'a, i64>, E> {
    let (rest, i) = recognize(pair(opt(tag("-")), digit1))(input)?;
    Ok((
        rest,
        ast_from_to(input, rest, i.fragment().parse::<i64>().unwrap()),
    ))
}

fn decimal<'a, E: ParseError<Span<'a>> + ContextError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Src<'a, f64>, E> {
    let (rest, i) = recognize(pair(opt(tag("-")), pair(digit1, pair(tag("."), digit1))))(input)?;
    Ok((
        rest,
        ast_from_to(input, rest, i.fragment().parse::<f64>().unwrap()),
    ))
}

fn command<'a>(
    command: &'a str,
    input: Span<'a>,
) -> IResult<Span<'a>, Src<'a, Object<'a>>, VerboseError<Span<'a>>> {
    let (rest, (_, obj)) = tuple((tag(command), preceded(ws1, object)))(input)?;
    //let create = Expr::Create(obj);
    Ok((rest, obj))
}

fn create<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    let (rest, obj) = command("create", input)?;
    let create = Expr::Create(obj);
    Ok((rest, ast_from_to(input, rest, create)))
}

fn query<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    let (rest, obj) = command("query", input)?;
    let create = Expr::Query(obj);
    Ok((rest, ast_from_to(input, rest, create)))
}

fn command_exp<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    alt((create, query))(input)
}

fn base<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    alt((
        command_exp,
        map(decimal, |f| wrap_ast(f.src, Expr::Dec(f))),
        map(integer, |i| wrap_ast(i.src, Expr::Int(i))),
        map(preceded(tag("@"), name), |name| {
            ast(name, Expr::Wildcard(name.fragment()))
        }),
        map(name, |t| ast(t, Expr::Var(&t))),
    ))(input)
}

fn paren<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    alt((
        delimited(
            preceded(ws0, tag("(")),
            preceded(ws0, expression),
            preceded(ws0, tag(")")),
        ),
        base,
    ))(input)
}

// todo this is where function calls as well as create and query go.
fn call_exp<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    let (rest, init) = paren(input)?;
    let mut rest = rest;
    let mut exp = init;
    while let Ok((r, (op, field))) = pair(tag("."), name)(rest) {
        rest = r;
        if op.fragment() == &"." {
            exp = ast_from_to(
                input,
                rest,
                Expr::Get(Box::new(exp), ast(field, field.fragment())),
            );
        } else {
            unreachable!()
        }
    }
    Ok((rest, exp))
}

fn unary_exp<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    let (rest, op) = opt(preceded(ws0, alt((tag("-"), tag("!")))))(input)?;
    match op {
        Some(op) => {
            if op.fragment() == &"-" {
                let (rest, exp) = preceded(ws0, unary_exp)(rest)?;
                Ok((
                    rest,
                    ast_from_to(input, rest, Expr::Op(ast(op, Op::Neg), Box::new(exp))),
                ))
            } else if op.fragment() == &"!" {
                let (rest, exp) = unary_exp(rest)?;
                Ok((
                    rest,
                    ast_from_to(input, rest, Expr::Op(ast(op, Op::Not), Box::new(exp))),
                ))
            } else {
                unreachable!()
            }
        }
        None => call_exp(input),
    }
}

fn mul_exp<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    let (rest, init) = unary_exp(input)?;
    let mut rest = rest;
    let mut exp = init;
    while let Ok((r, (op, val))) = pair(
        preceded(ws0, alt((tag("*"), tag("/"), tag("%")))),
        preceded(ws0, unary_exp),
    )(rest)
    {
        rest = r;
        if op.fragment() == &"*" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Mul), Box::new(exp), Box::new(val)),
            );
        } else if op.fragment() == &"/" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Div), Box::new(exp), Box::new(val)),
            );
        } else if op.fragment() == &"%" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Mod), Box::new(exp), Box::new(val)),
            );
        } else {
            unreachable!()
        }
    }
    Ok((rest, exp))
}

fn add_exp<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    let (rest, init) = mul_exp(input)?;
    let mut rest = rest;
    let mut exp = init;
    while let Ok((r, (op, val))) = pair(
        preceded(ws0, alt((tag("+"), tag("-")))),
        preceded(ws0, mul_exp),
    )(rest)
    {
        rest = r;
        if op.fragment() == &"+" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Add), Box::new(exp), Box::new(val)),
            );
        } else if op.fragment() == &"-" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Sub), Box::new(exp), Box::new(val)),
            );
        } else {
            unreachable!()
        }
    }
    Ok((rest, exp))
}

fn cmp_exp<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    let (rest, init) = add_exp(input)?;
    let mut rest = rest;
    let mut exp = init;
    while let Ok((r, (op, val))) = pair(
        preceded(ws0, alt((tag("<="), tag(">="), tag("<"), tag(">")))),
        preceded(ws0, add_exp),
    )(rest)
    {
        rest = r;
        if op.fragment() == &"<" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Lt), Box::new(exp), Box::new(val)),
            );
        } else if op.fragment() == &">" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Gt), Box::new(exp), Box::new(val)),
            );
        } else if op.fragment() == &"<=" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Lte), Box::new(exp), Box::new(val)),
            );
        } else if op.fragment() == &">=" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Gte), Box::new(exp), Box::new(val)),
            );
        } else {
            unreachable!()
        }
    }
    Ok((rest, exp))
}

fn eq_exp<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    let (rest, init) = cmp_exp(input)?;
    let mut rest = rest;
    let mut exp = init;
    while let Ok((r, (op, val))) = pair(
        preceded(ws0, alt((tag("=="), tag("!=")))),
        preceded(ws0, cmp_exp),
    )(rest)
    {
        rest = r;
        if op.fragment() == &"==" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Eq), Box::new(exp), Box::new(val)),
            );
        } else if op.fragment() == &"!=" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Neq), Box::new(exp), Box::new(val)),
            );
        } else {
            unreachable!()
        }
    }
    Ok((rest, exp))
}

fn and_exp<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    let (rest, init) = eq_exp(input)?;
    let mut rest = rest;
    let mut exp = init;
    while let Ok((r, (op, val))) = pair(preceded(ws0, tag("&&")), preceded(ws0, eq_exp))(rest) {
        rest = r;
        if op.fragment() == &"&&" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::And), Box::new(exp), Box::new(val)),
            );
        } else {
            unreachable!()
        }
    }
    Ok((rest, exp))
}

fn or_exp<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    let (rest, init) = and_exp(input)?;
    let mut rest = rest;
    let mut exp = init;
    while let Ok((r, (op, val))) = pair(preceded(ws0, tag("||")), preceded(ws0, and_exp))(rest) {
        rest = r;
        if op.fragment() == &"||" {
            exp = ast_from_to(
                input,
                rest,
                Expr::Op2(ast(op, Op2::Or), Box::new(exp), Box::new(val)),
            );
        } else {
            unreachable!()
        }
    }
    Ok((rest, exp))
}

fn expression<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Expr>, VerboseError<Span<'a>>> {
    or_exp(input)
}

fn field_and_type<'a>(
    input: Span<'a>,
) -> IResult<Span<'a>, Src<TypeField>, VerboseError<Span<'a>>> {
    let (rest, (name, _, kind)) =
        tuple((name, preceded(ws0, tag(":")), preceded(ws0, name)))(input)?;
    Ok((
        rest,
        ast_from_to(
            input,
            rest,
            TypeField {
                name: &name,
                kind: &kind,
            },
        ),
    ))
}

fn type_fields<'a>(
    input: Span<'a>,
) -> IResult<Span<'a>, Vec<Src<TypeField>>, VerboseError<Span<'a>>> {
    terminated(
        separated_list0(preceded(ws0, tag(",")), preceded(ws0, field_and_type)),
        preceded(ws0, opt(char(','))),
    )(input)
}

fn type_declaration<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Type>, VerboseError<Span<'a>>> {
    let (rest, (_, name, _, fields, _)) = tuple((
        tag("type"),
        preceded(ws1, name),
        preceded(ws0, char('{')),
        preceded(ws0, type_fields),
        preceded(ws0, char('}')),
    ))(input)?;
    let t = Type {
        name: &name,
        fields,
    };
    Ok((rest, ast_from_to(input, rest, t)))
}

fn field_and_expr<'a>(
    input: Span<'a>,
) -> IResult<Span<'a>, Src<ObjectField>, VerboseError<Span<'a>>> {
    let (rest, (name, _, value)) =
        tuple((name, preceded(ws0, tag(":")), preceded(ws0, expression)))(input)?;
    Ok((
        rest,
        ast_from_to(input, rest, ObjectField { name: &name, value }),
    ))
}

fn object_fields<'a>(
    input: Span<'a>,
) -> IResult<Span<'a>, Vec<Src<ObjectField>>, VerboseError<Span<'a>>> {
    terminated(
        separated_list0(preceded(ws0, tag(",")), preceded(ws0, field_and_expr)),
        preceded(ws0, opt(char(','))),
    )(input)
}

fn object<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Object>, VerboseError<Span<'a>>> {
    let (rest, (kind, _, fields, _)) = tuple((
        name,
        preceded(ws0, char('{')),
        preceded(ws0, object_fields),
        preceded(ws0, char('}')),
    ))(input)?;
    let o = Object {
        kind: &kind,
        fields,
    };
    Ok((rest, ast_from_to(input, rest, o)))
}

fn print_statement<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Stmt>, VerboseError<Span<'a>>> {
    let (rest, (_t, expr)) = tuple((tag("print"), preceded(ws1, expression)))(input)?;
    Ok((rest, ast_from_to(input, rest, Stmt::Print(expr))))
}

fn dot_chain<'a>(input: Span<'a>) -> IResult<Span<'a>, Vec<Src<&'a str>>, VerboseError<Span<'a>>> {
    let (rest, (name, more_names)) = tuple((
        name,
        many0(preceded(preceded(ws0, tag(".")), preceded(ws0, name))),
    ))(input)?;
    let mut names = vec![name];
    names.extend(more_names);
    let names: Vec<Src<&'a str>> = names
        .into_iter()
        .map(|name| Src {
            src: source_info(&name),
            node: name.fragment().clone(),
        })
        .collect();
    Ok((rest, names))
}

fn assignment_statement<'a>(
    input: Span<'a>,
) -> IResult<Span<'a>, Src<Stmt>, VerboseError<Span<'a>>> {
    let (rest, (target, expr)) = tuple((
        preceded(ws0, dot_chain),
        preceded(preceded(ws0, tag("=")), preceded(ws0, expression)),
    ))(input)?;
    let stmt = Stmt::Assign { target, expr };
    Ok((rest, ast_from_to(input, rest, stmt)))
}

fn expression_statement<'a>(
    input: Span<'a>,
) -> IResult<Span<'a>, Src<Stmt>, VerboseError<Span<'a>>> {
    let (rest, expr) = expression(input)?;
    let stmt = Stmt::Expression(expr);
    Ok((rest, ast_from_to(input, rest, stmt)))
}

fn statement<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Stmt>, VerboseError<Span<'a>>> {
    alt((print_statement, assignment_statement, expression_statement))(input)
}

fn variable_declaration<'a>(
    input: Span<'a>,
) -> IResult<Span<'a>, Src<VarDecl>, VerboseError<Span<'a>>> {
    let (rest, (_, name, expr)) = tuple((
        tag("var"),
        preceded(ws1, name),
        // optional expression
        opt(preceded(preceded(ws0, tag("=")), preceded(ws0, expression))),
    ))(input)?;
    Ok((
        rest,
        ast_from_to(input, rest, VarDecl { name: &name, expr }),
    ))
}

fn declaration<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Decl<'a>>, VerboseError<Span<'a>>> {
    context(
        "declaration",
        alt((
            map(variable_declaration, |vardecl| {
                wrap_ast(vardecl.src, Decl::VarDecl(vardecl))
            }),
            map(type_declaration, |typedef| {
                wrap_ast(typedef.src, Decl::Type(typedef))
            }),
            map(statement, |stmt| wrap_ast(stmt.src, Decl::Stmt(stmt))),
        )),
    )(input)
}

fn program<'a>(input: Span<'a>) -> IResult<Span<'a>, Src<Program<'a>>, VerboseError<Span<'a>>> {
    let (rest, decls) = terminated(
        many0(preceded(ws0, declaration)),
        preceded(ws0, context("declaration", eof)),
    )(input)?;
    Ok((rest, ast(input, decls)))
}

pub fn parse_program<'a>(input: &'a str) -> Result<Src<Program<'a>>, Vec<Error>> {
    let span = Span::new(input);
    program(span)
        .finish()
        .map(|(_rest, stmts)| {
            // todo: this should be an error but not a panic
            // if rest.fragment().len() > 0 {
            //     panic!("not all input consumed: {:?}", rest.fragment());
            // }
            stmts
        })
        .map_err(|e| convert_error(span, e))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Run the parser on the input span.
    /// If there's an error convert it and print it out.
    fn check_parse<'a, R>(
        span: Span<'a>,
        result: IResult<Span<'a>, R, VerboseError<Span<'a>>>,
    ) -> bool {
        let result = result
            .finish()
            .map(|(rest, stmts)| {
                if rest.fragment().len() > 0 {
                    panic!("not all input consumed: {:?}", rest.fragment());
                }
                stmts
            })
            .map_err(|e| convert_error(span, e));
        if let Err(ref errs) = result {
            for e in errs {
                match e {
                    Error::Parse {
                        line: _,
                        column: _,
                        message,
                    } => println!("ParseError {}", message),
                    Error::Runtime {
                        line: _,
                        column: _,
                        message,
                    } => println!("RuntimeError {}", message),
                }
            }
        }
        result.is_ok()
    }

    // Parse with the parser and assert that it parses successfully.
    #[macro_export]
    macro_rules! assert_parses {
        ($input:expr, $parser:expr) => {
            let span = Span::new($input);
            let result = $parser(span);
            assert!(check_parse(span, result));
        };
    }

    // Parse both expressions with the parser and assert that they return the same tree.
    // Used for checking precedence and associativity by comparing the same expression
    // with and without explicit parens.
    #[macro_export]
    macro_rules! assert_parses_same {
        ($input1:expr, $input2:expr, $parser:expr) => {
            let span1 = Span::new($input1);
            let result1 = $parser(span1);
            let span2 = Span::new($input2);
            let result2 = $parser(span2);
            assert!(check_parse(span1, result1.clone()));
            assert!(check_parse(span2, result2.clone()));
            assert!(result1.unwrap().1 == result2.unwrap().1);
        };
    }

    #[test]
    fn test_comment() {
        assert_parses!("#nothing special", comment);
        assert_parses!("# just a comment", comment);
    }

    #[test]
    fn test_whitespace() {
        assert_parses!("", ws0);
        assert_parses!("     ", ws0);
        assert_parses!("#comments are whitespace\n\n\n", ws1);
        assert_parses!(";;;;;;;;;", ws1);
        assert_parses!("\n", ws1);
        assert_parses!(";\n#comment\n", ws1);
    }

    #[test]
    fn test_integer() {
        assert_parses!("12345", integer);
        assert_parses!("-12345", integer);
        assert_parses!("0", integer);
        assert_parses!("-0", integer);
        assert_parses!("-0123", integer);
        assert_parses!("0123", integer);
    }

    #[test]
    fn test_decimal() {
        assert_parses!("12345.0", decimal);
        assert_parses!("-12345.0", decimal);
        assert_parses!("0.0", decimal);
        assert_parses!("-0.0", decimal);
        assert_parses!("-0.123", decimal);
        assert_parses!("01.23", decimal);
    }

    #[test]
    fn test_name() {
        assert_parses!("steve", name);
        assert_parses!("z33sh4n", name);
        assert_parses!("JeReMy", name);
        assert_parses!("_k_y_l_e", name);
    }

    #[test]
    fn test_field_and_type() {
        assert_parses!("foo: Bar", field_and_type);
        assert_parses!("foo :Bar", field_and_type);
        assert_parses!("foo:Bar", field_and_type);
        assert_parses!("foo\n:#wow\nBar", field_and_type);
    }

    #[test]
    fn test_type_fields() {
        assert_parses!("foo: Bar, wam: Baz", type_fields);
        assert_parses!("foo: Bar\n,\nwam: Baz", type_fields);
    }

    #[test]
    fn test_type_declaration() {
        assert_parses!("type Org{ wat:ohno , ok: go\n}", type_declaration);
        assert_parses!("type Org{wat:ohno,ok:go}", type_declaration);
        assert_parses!("type\nOrg { wat : ohno , ok : go }", type_declaration);
    }

    #[test]
    fn test_field_and_expr() {
        assert_parses!("foo: 1", field_and_expr);
        assert_parses!("foo :var", field_and_expr);
        assert_parses!("foo:@Foo", field_and_expr);
        assert_parses!("foo\n:#wow\ncreate Bar{}", field_and_expr);
    }

    #[test]
    fn test_object_fields() {
        assert_parses!("foo: Bar, wam: Baz", object_fields);
        assert_parses!("foo: Bar\n,\nwam: Baz", object_fields);
    }

    #[test]
    fn test_object() {
        assert_parses!("Org{ wat:ohno , ok: go\n}", object);
        assert_parses!("Org{wat:ohno,ok:go}", object);
        assert_parses!("Org { wat : ohno , ok : @Int }", object);
    }

    #[test]
    fn test_command() {
        assert_parses!("create Foo{x: 1}", create);
        assert_parses!("create Foo {\n  x: 1, y: 2}", create);
        assert_parses!("create Bar{x: @Int}", create);
        assert_parses!("query Foo{x: 1}", query);
        assert_parses!("query Foo{}", query);
        assert_parses!("query Bar{x: @Dec}", query);
    }

    #[test]
    fn test_exp() {
        assert_parses!("1", expression);
        assert_parses!("steve", expression);
        assert_parses!("steve||fred||1", expression);
        assert_parses!("steve  ||fred  ||1 ||\n2", expression);
        assert_parses!("( steve || fred )", expression);
        assert_parses_same!("1", "( 1 )", expression);
        assert_parses_same!("x || y || z || w", "((x || y) || z) || w", expression);
        assert_parses!("x && y == z || w", expression);
        assert_parses_same!("x && y == z || w", "(x && (y == z)) || w", expression);
        assert_parses!("x <= y", expression);
        assert_parses!("!x <= -y <= z", expression);
        assert_parses_same!("!x <= -y <= z", "((!x) <= (-y)) <= z", expression);
        assert_parses!("steve.o", expression);
        assert_parses!("s.t.e.v.e + a.b == z.z", expression);
        assert_parses_same!("a.b.c", "(a.b).c", expression);
        assert_parses!("query Bar{x: @Int}", expression);
        assert_parses!("query Bar{x: @Int}.x <= 12", expression);
    }

    #[test]
    fn test_print() {
        assert_parses!("print 1", print_statement);
        assert_parses!("print name", print_statement);
        assert_parses!("print @Int", print_statement);
        assert_parses!("print query Bar{x: @Int}", print_statement);
    }

    #[test]
    fn test_assignment() {
        assert_parses!("x = 1", assignment_statement);
        assert_parses!("y = name", assignment_statement);
        assert_parses!("z = @Int", assignment_statement);
        assert_parses!("x.y.z. q . wow. yes = @Var", assignment_statement);
        assert_parses_same!(
            "x.y.z. q . wow. yes = \n@Wat",
            "x.y.z.q.wow.yes=@Wat",
            assignment_statement
        );
    }

    #[test]
    fn test_statement() {
        assert_parses!("x = 1", statement);
        assert_parses!("print x", statement);
        assert_parses!("create Foo {}", statement);
    }

    #[test]
    fn test_variable_declaration() {
        assert_parses!("var y", variable_declaration);
        assert_parses!("var x = 1", variable_declaration);
        assert_parses!("var foo = create Foo{x: 1}", variable_declaration);
    }

    #[test]
    fn test_declaration() {
        assert_parses!("type Org{ wat:ohno , ok: go\n}", type_declaration);
        assert_parses!("var y", variable_declaration);
        assert_parses!("var x = 1", variable_declaration);
        assert_parses!("x = 1", statement);
        assert_parses!("print x", statement);
    }

    #[test]
    fn test_program() {
        let s = r#"
            type Org{ wat:ohno , ok: go }
            var foo = create Org{wat: 1, ok:go}
            print foo
            foo.wat = pp.ohno
            create Foo{} # just a top level expression
            x = 1
            print x
            foo.bar.baz = 1 + 2 <= 3 && 4==4 || create Wat{} ==6
            "#;
        assert_parses!(s, program);
    }
}

/*


type Repository {
    name: String
    owner: Organization
    stars: Int
}

type User {
    name: String
}

type Role {
    name: String
    user: User
    organization: Organization
}

loop 5 {
    var org = create Organization{
        name: @
        number_of_employees: @(this > 0 && this < 10)
    }
}

var user = create User {
    name: @
}

var rand_org = queryOne Organization {
    number_of_employees: @(this > 10)
}

var repo = create Repository {
    name: @
    owner: rand_org
    stars: @ @(this > 0 && this < 1000)
}

loop 5 {
    var rand_org = queryOne Organization {}
    rand_org.number_of_employees = rand_org.number_of_employees + 1
}

// this is a pretty good start, gonna just ignore the math and the constraints but can get started
on the rest.

*/

// A file is bunch of declarations.
// Type declarations,
// variable declaration,
// other statements and stuff.

// expressions is a huge piece of it and so is statements.
// Gonna start with like
// var x = 1;
// print x;

// need to write way better tests. I want to like specify a program, assert it's parsed, assert it compiles and
// check stuff about it's compile and run it and check output stuff.
