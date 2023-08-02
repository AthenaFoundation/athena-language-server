/// Appends formatted string to a `String`.
#[macro_export]
macro_rules! format_to {
    ($buf:expr) => ();
    ($buf:expr, $lit:literal $($arg:tt)*) => {
        { use ::std::fmt::Write as _; let _ = ::std::write!($buf, $lit $($arg)*); }
    };
}

/// Generates `From` impls for `Enum E { Foo(Foo), Bar(Bar) }` enums
///
/// # Example
///
/// ```rust
/// enum Adt {
///     Struct(Struct),
///     Union(Union),
///     Enum(Enum),
/// }
/// # struct Struct;
/// # struct Union;
/// # struct Enum;
/// util::impl_from!(Struct, Union, Enum for Adt);
/// ```
#[macro_export]
macro_rules! impl_from {
    ($($variant:ident$(<$V:ident>)?),* for $enum:ident) => {
        $(
            impl$(<$V>)? From<$variant$(<$V>)?> for $enum$(<$V>)? {
                fn from(it: $variant$(<$V>)?) -> $enum$(<$V>)? {
                    $enum::$variant(it)
                }
            }
        )*
    };
    ($($variant:ident($t:ty)),* for $enum:ident) => {
        $(
            impl From<$t> for $enum {
                fn from(it: $t) -> $enum {
                    $enum::$variant(it)
                }
            }
        )*
    };
}
