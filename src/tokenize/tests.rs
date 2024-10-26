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
fn check_tokenize_bool() {
    assert_tokenize("true", [TokenKind::Keyword(Keyword::True)]);
    assert_tokenize("false", [TokenKind::Keyword(Keyword::False)]);
}

#[test]
fn check_tokenize_symbols() {
    assert_tokenize(";", [TokenKind::Semicolon]);
    assert_tokenize(",", [TokenKind::Comma]);
    assert_tokenize("=", [TokenKind::Equals]);
}

#[test]
fn check_tokenize_delims() {
    assert_tokenize("(", [TokenKind::OpenDelim(Delim::Paren)]);
    assert_tokenize(")", [TokenKind::CloseDelim(Delim::Paren)]);
}

#[test]
fn check_tokenize_bin_operators() {
    assert_tokenize("+", [TokenKind::BinOperator(BinOperator::Add)]);
    assert_tokenize("-", [TokenKind::BinOperator(BinOperator::Sub)]);
    assert_tokenize("*", [TokenKind::BinOperator(BinOperator::Mul)]);
    assert_tokenize("/", [TokenKind::BinOperator(BinOperator::Div)]);
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
