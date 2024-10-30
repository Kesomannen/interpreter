use super::*;

#[test]
fn check_tokenize_ident() {
    assert_tokenize("print", [TokenKind::Ident("print".into())]);
    assert_tokenize("_private", [TokenKind::Ident("_private".into())]);
    assert_tokenize("🥞", [TokenKind::Ident("🥞".into())]);
}

#[test]
fn check_tokenize_string() {
    assert_tokenize(
        r#""hello, world""#,
        [TokenKind::String("hello, world".into())],
    );

    assert_tokenize(r#""""#, [TokenKind::String("".into())]);
}

#[test]
fn check_tokenize_int() {
    assert_tokenize("42", [TokenKind::Int(42)]);
}

#[test]
fn check_tokenize_keywords() {
    assert_tokenize("true", [TokenKind::Keyword(Keyword::True)]);
    assert_tokenize("false", [TokenKind::Keyword(Keyword::False)]);
    assert_tokenize("if", [TokenKind::Keyword(Keyword::If)]);
    assert_tokenize("else", [TokenKind::Keyword(Keyword::Else)]);
    assert_tokenize("return", [TokenKind::Keyword(Keyword::Return)]);
}

#[test]
fn check_tokenize_symbols() {
    assert_tokenize(";", [TokenKind::Semicolon]);
    assert_tokenize(",", [TokenKind::Comma]);
    assert_tokenize("=", [TokenKind::Equals]);
    assert_tokenize("|", [TokenKind::Pipe]);
}

#[test]
fn check_tokenize_delims() {
    assert_tokenize("(", [TokenKind::OpenDelim(Delim::Paren)]);
    assert_tokenize(")", [TokenKind::CloseDelim(Delim::Paren)]);
    assert_tokenize("{", [TokenKind::OpenDelim(Delim::Brace)]);
    assert_tokenize("}", [TokenKind::CloseDelim(Delim::Brace)]);
}

#[test]
fn check_tokenize_bin_operators() {
    assert_tokenize("+", [TokenKind::BinOperator(BinOperator::Add)]);
    assert_tokenize("-", [TokenKind::BinOperator(BinOperator::Sub)]);
    assert_tokenize("*", [TokenKind::BinOperator(BinOperator::Mul)]);
    assert_tokenize("/", [TokenKind::BinOperator(BinOperator::Div)]);
    assert_tokenize("&&", [TokenKind::BinOperator(BinOperator::And)]);
    assert_tokenize("||", [TokenKind::BinOperator(BinOperator::Or)]);
    assert_tokenize("==", [TokenKind::BinOperator(BinOperator::Eq)]);
    assert_tokenize("!=", [TokenKind::BinOperator(BinOperator::Ne)]);
    assert_tokenize(">=", [TokenKind::BinOperator(BinOperator::Gte)]);
    assert_tokenize(">", [TokenKind::BinOperator(BinOperator::Gt)]);
    assert_tokenize("<=", [TokenKind::BinOperator(BinOperator::Lte)]);
    assert_tokenize("<", [TokenKind::BinOperator(BinOperator::Lt)]);
}

#[test]
fn check_tokenize_comment() {
    assert_tokenize(
        "+ // this is a comment\n-",
        [
            TokenKind::BinOperator(BinOperator::Add),
            TokenKind::BinOperator(BinOperator::Sub),
        ],
    );
}

fn assert_tokenize(src: &str, expected: impl IntoIterator<Item = TokenKind>) {
    let tokens = tokenize(src).collect::<Vec<_>>();

    for (token, expected) in tokens
        .into_iter()
        .map(|res| res.unwrap().kind)
        .zip(expected)
    {
        assert_eq!(token, expected);
    }
}
