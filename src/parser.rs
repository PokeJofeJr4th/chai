use std::iter::Peekable;

use crate::lexer::token::Token;

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
        Token::Ident(id) if &*id == "fn" => todo!(),
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
