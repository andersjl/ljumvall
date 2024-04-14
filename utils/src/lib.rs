use regex::Regex;
use std::sync::OnceLock;

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

pub fn blacken(s: &str) -> String {
    static WHITE: OnceLock<Regex> = OnceLock::new();
    WHITE
        .get_or_init(|| Regex::new(r"\s*").unwrap())
        .replace_all(s, "")
        .to_string()
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

/// - leave an empty string alone
/// - if lead is negative remove leading slash if present
/// - if lead is positive add leading slash if missing
/// - if trail is negative remove trailing slash if present
/// - if trail is positive add trailing slash if missing
///
pub fn fix_slashes(s: &str, lead: i32, trail: i32) -> String {
    if s.is_empty() {
        return String::new();
    }
    let mut result = s.to_string();
    if lead < 0 && result.starts_with('/') {
        result.remove(0);
    }
    if lead > 0 && !result.starts_with('/') {
        result.insert(0, '/');
    }
    if trail < 0 && result.ends_with('/') {
        result.remove(result.len() - 1);
    }
    if trail > 0 && !result.ends_with('/') {
        result = result + "/";
    }
    result
}

/// Get a unix timestamp from a file path.
///
/// If the file is not found, the error text is
/// *forwarded low level error*`--`*path to file*.
///
pub fn timestamp(file: &str) -> Result<u64, String> {
    match std::fs::metadata(file) {
        Ok(data) => match data.modified() {
            Ok(system_time) => match system_time
                .duration_since(std::time::SystemTime::UNIX_EPOCH)
            {
                Ok(duration) => Ok(duration.as_secs()),
                Err(e) => Err(e.to_string() + "--" + file),
            },
            Err(e) => Err(e.to_string() + "--" + file),
        },
        Err(e) => Err(e.to_string() + "--" + file),
    }
}
