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
fn check_tokenize_symbols() {
    assert_tokenize("(", [TokenKind::OpenParen]);
    assert_tokenize(")", [TokenKind::CloseParen]);
    assert_tokenize(";", [TokenKind::Semicolon]);
    assert_tokenize(",", [TokenKind::Comma]);
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
