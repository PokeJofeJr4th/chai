use std::iter::Peekable;

use self::token::Token;

pub mod token;

macro_rules! multi_character_pattern {
    ($chars:ident $just:expr; $($char:expr => $eq:expr),*) => {
        match $chars.peek() {
            $(Some($char) => {
                $chars.next();
                $eq
            })*
            _ => $just,
        }
    };
}

const fn match_escape(esc: char) -> Option<char> {
    match esc {
        '\\' => Some('\\'),
        '\'' => Some('\''),
        'n' => Some('\n'),
        'r' => Some('\r'),
        '"' => Some('"'),
        't' => Some('\t'),
        _ => None,
    }
}

/// # Errors
pub fn tokenize(src: &str) -> Result<Vec<Token>, String> {
    let mut toks = Vec::new();
    let mut iter = src.chars().peekable();
    while iter.peek().is_some() {
        if let Some(tok) = inner_tokenize(&mut iter)? {
            toks.push(tok);
        }
    }
    Ok(toks)
}

#[allow(
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::too_many_lines
)]
fn inner_tokenize<T: Iterator<Item = char>>(
    chars: &mut Peekable<T>,
) -> Result<Option<Token>, String> {
    let Some(char) = chars.next() else {
        return Err("Unexpected end of file".into());
    };
    Ok(Some(match char {
        '{' => Token::LCurly,
        '}' => Token::RCurly,
        '(' => Token::LParen,
        ')' => Token::RParen,
        '[' => Token::LSquare,
        ']' => Token::RSquare,
        // '@' => Token::At,
        '=' => multi_character_pattern!(chars Token::Eq; '>' => Token::BigArrow),
        '+' => {
            multi_character_pattern!(chars Token::Plus; '=' => Token::PlusEq, '+' => Token::PlusPlus)
        }
        '-' => {
            multi_character_pattern!(chars Token::Tack; '=' => Token::TackEq, '-' => Token::TackTack, '>' => Token::Arrow)
        }
        '*' => multi_character_pattern!(chars Token::Star; '=' => Token::StarEq),
        '/' => multi_character_pattern!(chars Token::Slash; '=' => Token::SlashEq),
        '%' => multi_character_pattern!(chars Token::Percent; '=' => Token::PercEq),
        '!' => multi_character_pattern!(chars Token::Bang; '=' => Token::BangEq),
        '?' => multi_character_pattern!(chars Token::Question; '?' => Token::QuestionQuestion),
        '<' => multi_character_pattern!(chars Token::LCaret; '=' => Token::LCaretEq),
        '>' => {
            multi_character_pattern!(chars Token::RCaret; '=' => Token::RCaretEq)
        }
        '.' => Token::Dot,
        ':' => {
            multi_character_pattern!(chars Token::Colon; ':' => Token::ColonColon)
        }
        ';' => Token::Semicolon,
        ',' => Token::Comma,
        '"' => {
            let mut string_buf = String::new();
            while let Some(next) = chars.next() {
                if next == '"' {
                    break;
                }
                string_buf.push(next);
                if next == '\\' {
                    let escape = chars
                        .next()
                        .ok_or_else(|| String::from("Unexpected end of file"))?;
                    string_buf.push(
                        match_escape(escape).ok_or_else(|| {
                            format!(r#"Unsupported escape character: "\{escape}""#)
                        })?,
                    );
                }
            }
            Token::String(string_buf.into())
        }
        // ignore whitespace
        char if char.is_whitespace() => return Ok(None),
        char if char.is_ascii_alphanumeric() || char == '_' => {
            // get an identifier / number / range
            let mut identifier_buf = String::from(char);
            while let Some(next) = chars.peek() {
                if next.is_ascii_alphanumeric() || *next == '_' {
                    identifier_buf.push(
                        chars
                            .next()
                            .ok_or_else(|| String::from("Unexpected end of file"))?,
                    );
                } else {
                    break;
                }
            }
            return Ok(Some(match identifier_buf.parse() {
                Ok(int) => match chars.peek() {
                    Some('.') => {
                        chars.next();
                        let mut decimal_buf = String::new();
                        while let Some(next) = chars.peek() {
                            if next.is_ascii_digit() {
                                decimal_buf.push(*next);
                                chars.next();
                            } else {
                                break;
                            }
                        }
                        Token::Float(
                            int as f64
                                + decimal_buf
                                    .parse::<f64>()
                                    .map_err(|_| String::from("Expected number after `.`"))?
                                    / 10.0f64.powi(decimal_buf.len() as i32),
                        )
                    }
                    _ => Token::Int(int),
                },
                Err(_) => Token::Ident(identifier_buf.into()),
            }));
        }
        char => {
            // unexpected character
            return Err(format!("Unexpected character: `{char}`"));
        }
    }))
}
