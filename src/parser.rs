use std::iter::Peekable;

use crate::{lexer::token::Token, types::FieldType};

use self::syntax::{ImportTree, TopLevel};

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
pub fn parse(src: Vec<Token>) -> Result<Vec<TopLevel>, String> {
    let mut toks = src.into_iter().peekable();
    let mut syntax = Vec::new();
    while toks.peek().is_some() {
        syntax.push(inner_parse(&mut toks)?);
    }
    Ok(syntax)
}

fn inner_parse(src: &mut Peekable<impl Iterator<Item = Token>>) -> Result<TopLevel, String> {
    match_token!(src {
        Token::Ident(id) if &*id == "import" => {
            let import_tree = single_import_tree(src)?;
            match_token!(src {
                Token::Semicolon => 
                Ok(TopLevel::Import(import_tree))
            } "`;`")
        },
        Token::Ident(id) if &*id == "fn" => inner_parse_function(src),
    } "`import` or `fn`")
}

fn import_tree(src: &mut Peekable<impl Iterator<Item = Token>>) -> Result<Vec<ImportTree>, String> {
    match_token!(src {
        Token::LCurly => {
            let mut children = Vec::new();
            loop {
                children.push(single_import_tree(src)?);
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
            let children = import_tree(src)?;
            Ok(vec![ImportTree { current, children }])
        },
        Token::Star => {
            Ok(vec![ImportTree {current: "*".into(), children: Vec::new()}])
        }
    } "identifier, `*`, or `{`")
}

fn single_import_tree(
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
    let children = import_tree(src)?;
    Ok(ImportTree { current, children })
}

fn inner_parse_function(
    src: &mut Peekable<impl Iterator<Item = Token>>,
) -> Result<TopLevel, String> {
    let_token!(src => Token::Ident(name), "function name identifier");
    let_token!(src => Token::LParen, "`(`");

    let mut params = Vec::new();
    if src.peek() != Some(&Token::RParen) {
        loop {
            let ty = inner_parse_type(src)?;
            let_token!(src => Token::Ident(field_name), "parameter name identifier");
            params.push((ty, field_name));
            match_token! (src {
                Token::RParen => break,
                Token::Comma => Ok(())
            } "`)` or `,`")?;
        }
    }

    let return_type = if src.peek() == Some(&Token::Arrow) {
        src.next();
        Some(inner_parse_type(src)?)
    } else {
        None
    };

    Ok(TopLevel::Function {
        name,
        params,
        return_type,
    })
}

fn inner_parse_type(src: &mut Peekable<impl Iterator<Item = Token>>) -> Result<FieldType, String> {
    let mut class_buf = String::new();
    let mut ret_type = match_token!(src {
        Token::Ident(id) if &*id == "boolean" => Ok(FieldType::Boolean),
        Token::Ident(id) if &*id == "byte" => Ok(FieldType::Byte),
        Token::Ident(id) if &*id == "short" => Ok(FieldType::Short),
        Token::Ident(id) if &*id == "int" => Ok(FieldType::Int),
        Token::Ident(id) if &*id == "long" => Ok(FieldType::Long),
        Token::Ident(id) if &*id == "float" => Ok(FieldType::Float),
        Token::Ident(id) if &*id == "double" => Ok(FieldType::Double),
        Token::Ident(id) if &*id == "char" => Ok(FieldType::Char),
        Token::Ident(id) => {
            class_buf.push_str(&id);
            loop {
                match src.peek() {
                    Some(Token::Ident(id)) => class_buf.push_str(id),
                    Some(Token::Dot) => class_buf.push('.'),
                    _ => break,
                }
                src.next();
            }
            Ok(FieldType::Object(class_buf.into()))
        }
    } "type")?;
    while src.peek() == Some(&Token::LSquare) {
        src.next();
        let_token!(src => Token::RSquare, "`]`");
        ret_type = FieldType::Array(Box::new(ret_type));
    }
    Ok(ret_type)
}
