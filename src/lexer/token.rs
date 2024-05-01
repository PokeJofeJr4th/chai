use std::sync::Arc;
#[derive(Debug)]
pub enum Token {
    Ident(Arc<str>),
    String(Arc<str>),
    Int(i64),
    Float(f64),
    Plus,
    PlusEq,
    PlusPlus,
    Tack,
    TackEq,
    TackTack,
    Slash,
    SlashEq,
    Star,
    StarEq,
    Percent,
    PercEq,
    Dot,
    Eq,
    EqEq,
    Bang,
    BangEq,
    LParen,
    RParen,
    LCurly,
    RCurly,
    LSquare,
    RSquare,
    Colon,
    ColonColon,
    Comma,
    Semicolon,
    LCaret,
    LCaretEq,
    RCaret,
    RCaretEq,
    Question,
    QuestionQuestion,
    Arrow,
    BigArrow,
}
