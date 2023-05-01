//! A macro using thiserror to turn Display into an Error

/// A macro using thiserror to turn Display into an Error
///
/// The arguments are an error `struct` name and a format literal for display.
/// Both arguments are optional, by default `Error` and `"{0}"`, respectively.
///
/// ## Examples
///
/// `define_error!(MyError, "MyError: {0}");`
///
/// will expand to
/// ```text
/// #[derive(Clone, Debug, Eq, thiserror::Error, PartialEq)]
/// #[error("MyError: {0}")]
/// pub struct MyError(String);
/// impl MyError {
///     pub fn new<T: std::fmt::Display>(text: T) -> Self {
///         Self(text.to_string())
///     }
/// }
/// ```
/// An example using it together with [`anyhow`
/// ](https://docs.rs/anyhow/latest/anyhow) follows.
/// ```
/// use anyhow;
/// use ljumvall_error::define_error;
///
/// define_error! {
///     /// Documentation of your error type goes here
///     Hidden
/// }
///
/// define_error!(Other, "Other: {0}");  /// If you do not need documentation
/// define_error!("This: {0}");
///
/// fn hidden() -> anyhow::Result<()> {
///     Err(Hidden::new("everywhere"))?
/// }
///
/// fn other() -> anyhow::Result<()> {
///     Err(Other::new("there"))?
/// }
///
/// fn this(error: Option<anyhow::Error>) -> anyhow::Result<()> {
///     Err(match error {
///         Some(e) => Error::new(e),
///         None => Error::new("here"),
///     })?
/// }
///
/// assert_eq!(format!("{:?}", this(None)), "Err(This: here)");
/// assert_eq!(format!("{:?}", this(Some(other().err().unwrap()))), "Err(This: Other: there)");
/// assert_eq!(format!("{:?}", this(Some(hidden().err().unwrap()))), "Err(This: everywhere)");
/// ```
#[macro_export]
macro_rules! define_error {
    ($( #[$meta:meta] )*) => {
        define_error!($( #[$meta] )* Error, "{0}");
    };
    ($( #[$meta:meta] )* $id:ident) => {
        define_error!($( #[$meta] )* $id, "{0}");
    };
    ($( #[$meta:meta] )* $format:literal) => {
        define_error!($( #[$meta] )* Error, $format);
    };
    ($( #[$meta:meta] )* $id:ident, $format:literal) => {
        $( #[$meta] )*
        #[derive(Clone, Debug, Eq, thiserror::Error, PartialEq)]
        #[error($format)]
        pub struct $id(String);
        impl $id {
            pub fn new<T: std::fmt::Display>(text: T) -> Self {
                Self(text.to_string())
            }
        }
    };
}
