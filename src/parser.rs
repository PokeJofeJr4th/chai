use std::iter::Peekable;

use crate::{
    lexer::token::Token,
    parser::syntax::{BinaryOperator, UnaryOperator},
    types::{IRFieldType, InnerFieldType},
};

use self::syntax::{Expression, ImportTree, TopLevel};

pub mod syntax;

macro_rules! match_token {
    ($src:ident {$($tok:pat $(if $guard:expr)? => $val:expr$(,)?),+} $expected:literal) => {
        match $src.next() {
            $(
                Some($tok) $(if $guard)? => $val,
            )*
            Some(other) => Err(format!("Unexpected Token `{other:?}`; expected {}", $expected)),
            None => Err(String::from("Unexpected EOF"))
        }
    };
}

macro_rules! let_token {
    ($src:ident => $pat:pat, $expected:literal) => {
        let Some(nxt) = $src.next() else {
            return Err(String::from("Unexpected EOF"));
        };
        let $pat = nxt else {
            return Err(format!(
                "Unexpected Token `{nxt:?}`; expected {}",
                $expected
            ));
        };
    };
}

/// # Errors
/// # Panics
pub fn parse(src: Vec<Token>) -> Result<Vec<TopLevel>, String> {
    let mut toks = src.into_iter().peekable();
    let mut syntax = Vec::new();
    while toks.peek().is_some() {
        syntax.push(inner_parse(&mut toks)?);
        println!("{:?}", syntax.last().unwrap());
    }
    Ok(syntax)
}

fn inner_parse(src: &mut Peekable<impl Iterator<Item = Token>>) -> Result<TopLevel, String> {
    match_token!(src {
        Token::Ident(id) if &*id == "import" => {
            let import_tree = parse_import_node(src)?;
            match_token!(src {
                Token::Semicolon => 
                Ok(TopLevel::Import(import_tree))
            } "`;`")
        },
        Token::Ident(id) if &*id == "fn" => parse_function(src),
    } "`import` or `fn`")
}

fn parse_import_tree(
    src: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<Vec<ImportTree>, String> {
    match_token!(src {
        Token::LCurly => {
            let mut children = Vec::new();
            loop {
                children.push(parse_import_node(src)?);
                match_token!(src {
                    Token::Comma => Ok(()),
                    Token::RCurly => break
                } "`,` or `}`")?;
            }
            Ok(children)
        },
        Token::Ident(current) => {
            let Some(Token::Dot) = src.peek() else {
                return Ok(vec![ImportTree {
                    current,
                    children: Vec::new(),
                }]);
            };
            src.next();
            let children = parse_import_tree(src)?;
            Ok(vec![ImportTree { current, children }])
        },
        Token::Star => {
            Ok(vec![ImportTree {current: "*".into(), children: Vec::new()}])
        }
    } "identifier, `*`, or `{`")
}

fn parse_import_node(
    src: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<ImportTree, String> {
    let_token!(src => Token::Ident(current), "identifier");
    let Some(Token::Dot) = src.peek() else {
        return Ok(ImportTree {
            current,
            children: Vec::new(),
        });
    };
    src.next();
    let children = parse_import_tree(src)?;
    Ok(ImportTree { current, children })
}

fn parse_function(src: &mut Peekable<impl Iterator<Item = Token>>) -> Result<TopLevel, String> {
    let_token!(src => Token::Ident(name), "function name identifier");
    let_token!(src => Token::LParen, "`(`");

    let mut params = Vec::new();
    if src.peek() != Some(&Token::RParen) {
        loop {
            let ty = parse_type(src)?;
            let_token!(src => Token::Ident(field_name), "parameter name identifier");
            params.push((ty, field_name));
            match_token! (src {
                Token::RParen => break,
                Token::Comma => Ok(())
            } "`)` or `,`")?;
        }
    }

    println!("{params:?}");

    let return_type = if src.peek() == Some(&Token::Arrow) {
        src.next();
        Some(parse_type(src)?)
    } else {
        None
    };

    let body = parse_item(src)?;

    Ok(TopLevel::Function {
        name,
        params,
        return_type,
        body,
    })
}

fn parse_type(src: &mut Peekable<impl Iterator<Item = Token>>) -> Result<IRFieldType, String> {
    let ty = match_token!(src {
        Token::Ident(id) if &*id == "boolean" => Ok(InnerFieldType::Boolean),
        Token::Ident(id) if &*id == "byte" => Ok(InnerFieldType::Byte),
        Token::Ident(id) if &*id == "short" => Ok(InnerFieldType::Short),
        Token::Ident(id) if &*id == "int" => Ok(InnerFieldType::Int),
        Token::Ident(id) if &*id == "long" => Ok(InnerFieldType::Long),
        Token::Ident(id) if &*id == "float" => Ok(InnerFieldType::Float),
        Token::Ident(id) if &*id == "double" => Ok(InnerFieldType::Double),
        Token::Ident(id) if &*id == "char" => Ok(InnerFieldType::Char),
        Token::Ident(id) => {
            let mut base = String::new();
            base.push_str(&id);
            loop {
                match src.peek() {
                    Some(Token::Ident(id)) => base.push_str(id),
                    Some(Token::Dot) => base.push('.'),
                    _ => break,
                }
                src.next();
            }
            let mut generics = Vec::new();
            if src.next_if_eq(&Token::LCaret).is_some() && src.next_if_eq(&Token::RCaret).is_none(){
                loop {
                    generics.push(parse_type(src)?);
                    match_token!(src {
                        Token::RCaret => break,
                        Token::Comma => Ok(()),
                    } "`>` or `,`")?;
                }
            }
            Ok(InnerFieldType::Object { base: base.into(), generics })
        },
        Token::LParen => {
            let mut parts = Vec::new();
            if src.peek() == Some(&Token::RParen) {
                src.next();
            } else {
                loop {
                    let ty = parse_type(src)?;
                    parts.push(ty);
                    match_token!(src {
                        Token::Comma => Ok(()),
                        Token::RParen => break,
                    } "`,` or `)`")?;
                }
            }
            Ok(InnerFieldType::Tuple(parts))
        }
    } "type")?;
    let mut array_depth = 0;
    while src.peek() == Some(&Token::LSquare) {
        src.next();
        let_token!(src => Token::RSquare, "`]`");
        array_depth += 1;
    }
    Ok(IRFieldType { ty, array_depth })
}

const BINARY_OPS: &[&[(Token, BinaryOperator)]] = &[
    &[
        (Token::Eq, BinaryOperator::Set),
        (Token::PlusEq, BinaryOperator::AddEq),
        (Token::TackEq, BinaryOperator::SubEq),
        (Token::StarEq, BinaryOperator::MulEq),
        (Token::SlashEq, BinaryOperator::DivEq),
        (Token::PercEq, BinaryOperator::ModEq),
        (Token::AndEq, BinaryOperator::BitAndEq),
        (Token::OrEq, BinaryOperator::BitOrEq),
        (Token::XorEq, BinaryOperator::XorEq),
    ],
    &[
        (Token::EqEq, BinaryOperator::Eq),
        (Token::BangEq, BinaryOperator::Ne),
        (Token::LCaret, BinaryOperator::Lt),
        (Token::RCaret, BinaryOperator::Gt),
        (Token::LCaretEq, BinaryOperator::Le),
        (Token::RCaretEq, BinaryOperator::Ge),
    ],
    &[
        (Token::Plus, BinaryOperator::Add),
        (Token::Tack, BinaryOperator::Sub),
    ],
    &[
        (Token::Star, BinaryOperator::Mul),
        (Token::Slash, BinaryOperator::Div),
        (Token::Percent, BinaryOperator::Mod),
    ],
    &[(Token::Dot, BinaryOperator::Dot)],
];

fn parse_expr_greedy(
    src: &mut Peekable<impl Iterator<Item = Token>>,
    priority: usize,
) -> Result<Expression, String> {
    if priority >= BINARY_OPS.len() {
        return parse_item(src);
    }
    let mut start = parse_expr_greedy(src, priority + 1)?;
    let ops = BINARY_OPS[priority];
    'outer: loop {
        let Some(next_tok) = src.peek() else { break };
        for (tok, op) in ops {
            if tok != next_tok {
                continue;
            }
            src.next();
            start = Expression::BinaryOperation(
                Box::new(start),
                *op,
                Box::new(parse_expr_greedy(src, priority + 1)?),
            );
            continue 'outer;
        }
        break;
    }
    if priority == 4 {
        loop {
            match src.peek() {
                Some(Token::PlusPlus) => {
                    start = Expression::BinaryOperation(
                        Box::new(start),
                        BinaryOperator::AddEq,
                        Box::new(Expression::Int(1)),
                    );
                    src.next();
                }
                Some(Token::TackTack) => {
                    start = Expression::BinaryOperation(
                        Box::new(start),
                        BinaryOperator::SubEq,
                        Box::new(Expression::Int(1)),
                    );
                    src.next();
                }
                _ => break,
            }
        }
    }
    Ok(start)
}

fn parse_expr(src: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression, String> {
    let start = parse_func_call(src)?;
    if src.peek() != Some(&Token::Question) {
        return Ok(start);
    }
    src.next();
    let if_true = parse_expr(src)?;
    let_token!(src => Token::Colon, "`:` (ternary else)");
    let if_false = parse_expr(src)?;
    Ok(Expression::If {
        condition: Box::new(start),
        body: Box::new(if_true),
        else_body: Some(Box::new(if_false)),
    })
}

fn parse_func_call(src: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression, String> {
    let mut lhs = parse_expr_greedy(src, 1)?;
    loop {
        match src.peek() {
            Some(Token::LParen) => {
                src.next();
                let mut args = Vec::new();
                if src.peek() == Some(&Token::RParen) {
                    src.next();
                } else {
                    loop {
                        let next_arg = parse_expr(src)?;
                        args.push(next_arg);
                        match_token!(src {
                            Token::Comma => {Ok(())},
                            Token::RParen => break,
                        } "`,` or `)`")?;
                    }
                }
                lhs = Expression::FunctionCall {
                    function: Box::new(lhs),
                    args,
                };
            }
            Some(Token::LSquare) => {
                src.next();
                let idx = parse_expr(src)?;
                let_token!(src => Token::RSquare, "`]`");
                lhs = Expression::BinaryOperation(
                    Box::new(lhs),
                    BinaryOperator::Index,
                    Box::new(idx),
                );
            }
            _ => break,
        }
    }
    Ok(lhs)
}

fn parse_item(src: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Expression, String> {
    let item = match_token!(src {
        Token::Bang => Ok(Expression::UnaryOperation(UnaryOperator::Not, Box::new(parse_func_call(src)?))),
        Token::Tack => Ok(Expression::UnaryOperation(UnaryOperator::Minus, Box::new(parse_func_call(src)?))),
        Token::String(string_lit) => Ok(Expression::String(string_lit)),
        Token::Int(int_lit) => Ok(Expression::Int(int_lit)),
        Token::Float(float_lit) => Ok(Expression::Float(float_lit)),
        Token::Ident(let_kw) if &*let_kw == "let" => {
            let ty = parse_type(src)?;
            let_token!(src => Token::Ident(var), "variable name");
            let_token!(src => Token::Eq, "`=`");
            let value = Box::new(parse_expr(src)?);
            Ok(Expression::Let { var, ty, value })
        },
        Token::Ident(for_kw) if &*for_kw == "for" => {
            let ty = parse_type(src)?;
            let_token!(src => Token::Ident(var), "identifier");
            let_token!(src => Token::Ident(in_kw), "`in`");
            if &*in_kw != "in" {
                return Err(format!("Unexpected token `{in_kw}`; expected `in`"));
            }
            let range = Box::new(parse_expr(src)?);
            let body = Box::new(parse_expr(src)?);
            Ok(Expression::For { ty, var, range, body })
        },
        Token::Ident(for_kw) if &*for_kw == "if" => {
            let condition = Box::new(parse_expr(src)?);
            let body = Box::new(parse_expr(src)?);
            let else_body = if let Some(Token::Ident(id)) = src.peek() {
                if &**id == "else" {
                    src.next();
                    Some(Box::new(parse_expr(src)?))
                } else {
                    None
                }
            } else {
                None
            };
            Ok(Expression::If { condition, body, else_body })
        },
        Token::Ident(try_kw) if &*try_kw == "try" => {
            let body = Box::new(parse_expr(src)?);
            Ok(Expression::UnaryOperation(UnaryOperator::Try, body))
        },
        Token::Ident(identifier) => Ok(Expression::Ident(identifier)),
        Token::LParen => {
            let mut tuple_items = Vec::new();
            if src.peek() == Some(&Token::RParen) {
                src.next();
            } else {
                loop {
                    let expr = parse_expr(src)?;
                    tuple_items.push(expr);
                    match_token!(src {
                        Token::Comma => Ok(()),
                        Token::RParen => break,
                    } "`,` or `)`")?;
                }
            }
            Ok(if tuple_items.len() == 1 {
                tuple_items.pop().unwrap()
            } else {
                Expression::Tuple(tuple_items)
            })
        },
        Token::LCurly => {
            let mut statements = Vec::new();
            let ret = loop {
                let stmt = parse_expr(src)?;
                println!("{stmt:?}");
                match_token!(src {
                    Token::Semicolon => Ok(()),
                    Token::RCurly => break Some(Box::new(stmt)),
                } "`;` or `}`")?;
                statements.push(stmt);
                if src.peek() == Some(&Token::RCurly) {
                    src.next();
                    break None;
                }
            };
            if statements.is_empty() && ret.is_some() {
                Ok(*ret.unwrap())
            } else {
                Ok(Expression::Block { statements, ret })
            }
        }
    } "expression")?;
    println!("{item:?}");
    Ok(item)
}
