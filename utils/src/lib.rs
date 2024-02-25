/// See <https://stackoverflow.com/questions/28028854/how-do-i-match-enum-values-with-an-integer>
#[macro_export]
macro_rules! back_to_enum {
    ($(#[$meta:meta])* $vis:vis enum $name:ident {
        $($(#[$vmeta:meta])* $vname:ident $(= $val:expr)?,)*
    }) => {
        $(#[$meta])*
        $vis enum $name {
            $($(#[$vmeta])* $vname $(= $val)?,)*
        }

        impl std::convert::TryFrom<i32> for $name {
            type Error = ();

            fn try_from(v: i32) -> Result<Self, Self::Error> {
                match v {
                    $(x if x == $name::$vname as i32 => Ok($name::$vname),)*
                    _ => Err(()),
                }
            }
        }
    }
}

/// For output of the first `max` characters of a potentially long string.
///
pub fn first_n_chars(s: &str, max: usize) -> &str {
    match s.char_indices().nth(max) {
        None => s,
        Some((idx, _)) => &s[..idx],
    }
}

/// For output of the first `max` characters of a potentially long `s` with
/// an `ellipsis` indicating missing characters.
///
/// If ``s` is longer than `max`, `s` is truncated to `max minus the number of
/// `char`s in `ellipsis` and `ellipsis` is appended.
///
pub fn first_n_chars_ellipsis(s: &str, max: usize, ellipsis: &str) -> String {
    let elen = ellipsis.chars().count();
    if elen >= max {
        s.to_string()
    } else {
        match s.char_indices().nth(max) {
            None => s.to_string(),
            Some(_) => {
                String::from(
                    &s[..s.char_indices().nth(max - elen).unwrap().0],
                ) + ellipsis
            }
        }
    }
}
