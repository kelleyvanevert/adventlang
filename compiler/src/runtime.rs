use std::borrow::Cow;

pub fn str_concat(a: Cow<str>, b: Cow<str>) -> Cow<'static, str> {
    format!("{a}{b}").into()
}

pub fn str_len(a: Cow<str>) -> usize {
    a.len()
}
